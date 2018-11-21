{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Construction of the symbol table and evaluation of constants.
module Rbsc.TypeChecker.ModelInfo
    ( getModelInfo
    ) where


import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set


import Rbsc.Config

import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed   (HasConstants (..), LSomeExpr,
                                      SomeExpr (..))
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped (Enumeration (..), LExpr,
                                      ModuleInstance (..), Parameter (..),
                                      TypeSetDef (..), UConstant, UFunction,
                                      UModuleInstance, UParameter, UType,
                                      UVarDecl, UVarType)
import qualified Rbsc.Syntax.Untyped as U

import           Rbsc.TypeChecker.ComponentTypes
import           Rbsc.TypeChecker.Dependencies
import           Rbsc.TypeChecker.Expr
import           Rbsc.TypeChecker.Identifiers
import qualified Rbsc.TypeChecker.Internal       as TC


data BuilderState = BuilderState
    { _modelInfo        :: !ModelInfo
    , _moduleInstances  :: Map TypeName (Map Name [UModuleInstance])
    , _bsRecursionDepth :: RecursionDepth
    }

makeLenses ''BuilderState

instance HasRecursionDepth BuilderState where
    recursionDepth = bsRecursionDepth

instance HasConstants BuilderState where
    constants = modelInfo.constants


-- | Construct the 'ModelInfo' for a given 'Model'.
getModelInfo
    :: (MonadReader r (t Result), HasRecursionDepth r, MonadTrans t)
    => U.Model
    -> t Result (ModelInfo, Map TypeName [UModuleInstance])
getModelInfo m = do
    idents <- lift (fromEither (identifierDefs m))
    deps   <- lift (fromEither' (sortDefinitions m idents))

    depth  <- view recursionDepth
    result <- lift (runBuilder (traverse addDependency deps) depth)

    lift (validateComponentTypes (view (_1.componentTypes) result) m)

    return result


addDependency :: Dependency -> Builder ()
addDependency = \case
    DepDefinition def -> case def of
        DefConstant c  -> addConstant c
        DefFunction f  -> addFunction f
        DefLabel       -> return ()
        DefGlobal decl -> addVariable Global decl
        DefLocal tyName moduleName decl ->
            addLocalVariable tyName moduleName decl
        DefComponentType t -> addComponentType t
        DefTypeSet s       -> addTypeSet s
        DefComponent c     -> addComponents c
        DefModule _        -> return ()
    DepFunctionSignature f    -> addFunctionSignature f
    DepModuleInstantiation mi -> addModuleInstance mi


addConstant :: UConstant -> Builder ()
addConstant (U.Constant (Loc name _) msTy e) = do
    SomeExpr e' ty <- case msTy of
        Just sTy -> do
            -- if an explicit type annotation is given, check if type of
            -- the definition matches
            Some ty <- fromSyntaxType sTy
            e' <- typeCheckExpr ty e
            return (SomeExpr e' ty)
        Nothing -> runTypeChecker (tcExpr e)

    Dict <- return (dictShow ty)
    v <- evalConstDefExpr (e' `withLocOf` e)

    insertSymbol Global name (Some ty)
    insertConstant name (SomeExpr (T.Literal v ty) ty)


addFunctionSignature :: UFunction -> Builder ()
addFunctionSignature (U.Function (Loc name _) params sTy _) = do
    paramTys <- traverse (fromSyntaxType . U.paramType) params
    tyResult <- fromSyntaxType sTy
    insertSymbol Global name (foldr mkTyFunc tyResult paramTys)
  where
    mkTyFunc (Some a) (Some b) = Some (a --> b)


addFunction :: UFunction -> Builder ()
addFunction (U.Function (Loc name _) params sTy body) = do
    paramSyms <- traverse paramToSym params
    tyResult  <- fromSyntaxType sTy

    f <- runTypeChecker (tcFunctionDef paramSyms tyResult body)
    insertConstant name f
  where
    paramToSym (U.Parameter n psTy) = do
        ty <- fromSyntaxType psTy
        return (unLoc n, ty)


addLocalVariable :: TypeName -> Name -> UVarDecl -> Builder ()
addLocalVariable tyName moduleName (U.VarDecl (Loc name _) vTy _) = do
    args <- getArguments
    (ty, mRange) <- withConstants args (fromSyntaxVarType vTy)

    insertSymbol (Local tyName) name ty
    insertRange (Local tyName) name mRange
  where
    getArguments :: Builder [(Name, LSomeExpr)]
    getArguments = do
        mi <- use (moduleInstances.at tyName._Just.at moduleName)
        return $ case mi of
            -- There can only be one module instance for a given local
            -- variable, since local variables names are unique per
            -- component type. If there would be more than one module
            -- instantiation for the same module, their local variables
            -- would clash.
            Just [inst] -> view U.miArgs inst
            Just _      -> error $
                "addLocalVariable: more than one instance for " ++
                show tyName ++ "." ++ show name
            Nothing     -> []

    -- | Temporarily add a list of constants to the 'ModelInfo'. The given
    -- 'Builder' action should not make any changes to the symbol table or
    -- the list of constants, as they will be lost (read access is fine
    -- however).
    withConstants :: [(Name, LSomeExpr)] -> Builder a -> Builder a
    withConstants cs m = do
        symTable <- use (modelInfo.symbolTable)
        consts   <- use (modelInfo.constants)

        for_ cs $ \(constName, Loc e@(SomeExpr _ ty) _) -> do
            modelInfo.symbolTable.at (ScopedName Global constName) ?= Some ty
            modelInfo.constants.at constName ?= e

        res <- m

        modelInfo.symbolTable .= symTable
        modelInfo.constants   .= consts

        return res


addVariable :: Scope -> UVarDecl -> Builder ()
addVariable sc (U.VarDecl (Loc name _) vTy _) = do
    (ty, mRange) <- fromSyntaxVarType vTy
    insertSymbol sc name ty
    insertRange sc name mRange


addComponentType :: ComponentTypeDef -> Builder ()
addComponentType = \case
    TypeDefNatural (U.NaturalTypeDef (Loc name _)) ->
        insertComponentType name NaturalType
    TypeDefRole (U.RoleTypeDef (Loc name _) playerTyNames) ->
        insertComponentType
            name
            (RoleType (Set.fromList (fmap unLoc playerTyNames)))
    TypeDefCompartment (U.CompartmentTypeDef (Loc name _) multiRoleLists) -> do
        roleRefLists <- traverse (traverse toRoleRef) multiRoleLists
        insertComponentType name (CompartmentType roleRefLists)
  where
    toRoleRef (U.MultiRole (Loc tyName _) mBounds) = case mBounds of
        Nothing -> return (RoleRef tyName (1, 1))
        Just (lower, upper) -> do
            lower' <- evalIntExpr lower
            upper' <- evalIntExpr upper
            checkCardinalities lower upper lower' upper'
            return (RoleRef tyName (lower', upper'))

    checkCardinalities lower upper lower' upper'
        | lower' < 0 = throw (getLoc lower) (InvalidLowerBound lower')
        | upper' < lower' =
            throw
                (getLoc lower <> getLoc upper)
                (InvalidCardinalities lower' upper')
        | otherwise = return ()


addTypeSet :: TypeSetDef -> Builder ()
addTypeSet (TypeSetDef (Loc name _) tyNames) =
    modelInfo.typeSets.at name ?= Set.fromList (fmap unLoc (toList tyNames))


addComponents :: ComponentDef -> Builder ()
addComponents (ComponentDef (Loc name _) (Loc tyName _) mLen) = case mLen of
    -- add component array
    Just len -> do
        len' <- evalIntExpr len
        let tyArray = TyArray (max 0 len') tyComponent
        insertSymbol Global name (Some tyArray)

    -- add single component
    Nothing -> insertSymbol Global name (Some tyComponent)
  where
    tyComponent = TyComponent (Set.singleton tyName)


addModuleInstance :: ModuleInstantiationDep -> Builder ()
addModuleInstance ModuleInstantiationDep {..} = do
    checkArity
    let args = zip midArgs (U.modParams midModule)
    args' <- traverse evalArgument args

    let moduleName = unLoc (U.modName midModule)
        inst       = ModuleInstance moduleName args' (U.modBody midModule)

    modifying moduleInstances $
        Map.insertWith (Map.unionWith (++))
            midTypeName
            (Map.singleton moduleName [inst])
  where
    checkArity = do
        let numArgs   = length midArgs
            numParams = length (U.modParams midModule)
        if numArgs == numParams
            then return ()
            else throw midRegion (WrongNumberOfArguments numParams numArgs)


    evalArgument :: (LExpr, UParameter) -> Builder (Name, LSomeExpr)
    evalArgument (arg, param) = do
        let name = unLoc (paramName param)

        Some ty <- fromSyntaxType (paramType param)
        arg'    <- typeCheckExpr ty arg
        val     <- evalExpr (arg' `withLocOf` arg)
        Dict    <- return (dictShow ty)

        return (name, SomeExpr (T.Literal val ty) ty `withLocOf` arg)


fromSyntaxType :: UType -> Builder (Some Type)
fromSyntaxType = \case
    U.TyBool   -> return (Some TyBool)
    U.TyInt    -> return (Some TyInt)
    U.TyDouble -> return (Some TyDouble)
    U.TyAction -> return (Some TyAction)
    U.TyComponent tySet -> do
        compTys <- use (modelInfo.componentTypes)
        tySetDefs <- use (modelInfo.typeSets)
        tySet' <- lift (fromEither' (normalizeTypeSet compTys tySetDefs tySet))
        return (Some (TyComponent tySet'))
    U.TyArray size sTy -> do
        sizeVal <- evalIntExpr size
        Some ty <- fromSyntaxType sTy
        return (Some (TyArray sizeVal ty))
    U.TyFunc sTyL sTyR -> do
        Some tyL <- fromSyntaxType sTyL
        Some tyR <- fromSyntaxType sTyR
        return (Some (tyL --> tyR))


fromSyntaxVarType :: UVarType -> Builder (Some Type, Maybe (Int, Int))
fromSyntaxVarType = \case
    U.VarTyBool ->
        return (Some TyBool, Nothing)
    U.VarTyInt (lower, upper) -> do
        lowerVal <- evalIntExpr lower
        upperVal <- evalIntExpr upper
        return (Some TyInt, Just (lowerVal, upperVal))
    U.VarTyEnum (Enumeration names) ->
        return (Some TyInt, Just (0, length names - 1))
    U.VarTyArray size vTy -> do
        sizeVal <- evalIntExpr size
        (Some ty, mRange) <- fromSyntaxVarType vTy
        return (Some (TyArray sizeVal ty), mRange)


type Builder a = StateT BuilderState Result a


runBuilder
    :: Builder a
    -> RecursionDepth
    -> Result (ModelInfo, Map TypeName [UModuleInstance])
runBuilder m depth = do
    BuilderState mi insts _ <- execStateT m initial
    let insts' = Map.map (concat . Map.elems) insts
    return (mi, insts')
  where
    initial = BuilderState emptyModelInfo Map.empty depth


evalIntExpr :: Loc U.Expr -> Builder Int
evalIntExpr e = do
    e' <- typeCheckExpr TyInt e
    evalExpr (e' `withLocOf` e)


evalExpr :: Loc (T.Expr t) -> Builder t
evalExpr e = do
    env <- get
    runReaderT (eval e) env


evalConstDefExpr :: Loc (T.Expr t) -> Builder t
evalConstDefExpr e = do
    env <- get
    runReaderT (evalConstDef e) env


typeCheckExpr :: Type t -> Loc U.Expr -> Builder (T.Expr t)
typeCheckExpr ty e = runTypeChecker (e `hasType` ty)


runTypeChecker :: TC.TypeChecker a -> Builder a
runTypeChecker m = do
    info  <- use modelInfo
    depth <- use recursionDepth
    lift (TC.runTypeChecker m info depth)


insertSymbol :: Scope -> Name -> Some Type -> Builder ()
insertSymbol sc name ty =
    modelInfo.symbolTable.at (ScopedName sc name) .= Just ty


insertRange :: Scope -> Name -> Maybe (Int, Int) -> Builder ()
insertRange sc name mRange =
    modelInfo.rangeTable.at (ScopedName sc name) .= mRange


insertConstant :: Name -> SomeExpr -> Builder ()
insertConstant name e = modelInfo.constants.at name .= Just e


insertComponentType :: TypeName -> ComponentType -> Builder ()
insertComponentType tyName ty = modelInfo.componentTypes.at tyName .= Just ty
