{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Construction of the symbol table and evaluation of constants.
module Rbsc.TypeChecker.ModelInfo
    ( getModelInfo
    ) where


import Control.Lens
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Set as Set


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

import           Rbsc.Syntax.Typed   (HasConstants (..), SomeExpr (..),
                                      TConstant, TType)
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped (Enumeration (..), UConstant, UFunction,
                                      UType, UVarDecl, UVarType)
import qualified Rbsc.Syntax.Untyped as U

import           Rbsc.TypeChecker.ComponentTypes
import           Rbsc.TypeChecker.Dependencies
import           Rbsc.TypeChecker.Expr
import           Rbsc.TypeChecker.Identifiers
import qualified Rbsc.TypeChecker.Internal       as TC


data BuilderState = BuilderState
    { _modelInfo        :: !ModelInfo
    , _constantDefs     :: [TConstant]
    , _bsRecursionDepth :: RecursionDepth
    }

makeLenses ''BuilderState

instance HasRecursionDepth BuilderState where
    recursionDepth = bsRecursionDepth

instance HasConstants BuilderState where
    constants = modelInfo.constants


-- | Construct the 'ModelInfo' for a given 'Model'. Since evaluation of
-- constants requires type checking the constant definitions, the checked
-- definitions are returned as well.
getModelInfo ::
       (MonadReader r (t Result), HasRecursionDepth r, MonadTrans t)
    => U.Model
    -> t Result (ModelInfo, [TConstant])
getModelInfo m = do
    idents <- lift (fromEither (identifierDefs m))
    deps   <- lift (fromEither' (sortDefinitions idents))

    depth  <- view recursionDepth
    result <- lift (runBuilder (traverse addDependency deps) depth)

    lift (validateComponentTypes (view (_1.componentTypes) result) m)

    return result


addDependency :: Dependency -> Builder ()
addDependency = \case
    DepDefinition def -> case def of
        DefConstant c        -> addConstant c
        DefFunction f        -> addFunction f
        DefLabel             -> return ()
        DefGlobal decl       -> addVariable Global decl
        DefLocal tyName decl -> addVariable (Local tyName) decl
        DefComponentType t   -> addComponentType t
        DefComponent c       -> addComponents c
    DepFunctionSignature f -> addFunctionSignature f


addConstant :: UConstant -> Builder ()
addConstant (U.Constant (Loc name rgn) msTy e) = do
    (msTy', SomeExpr e' ty) <- case msTy of
        Just sTy -> do
            -- if an explicit type annotation is given, check if type of
            -- the definition matches
            (sTy', Some ty) <- fromSyntaxType sTy
            e' <- typeCheckExpr ty e
            return (Just sTy', SomeExpr e' ty)
        Nothing -> do
            e' <- runTypeChecker (tcExpr e)
            return (Nothing, e')

    Dict <- return (dictShow ty)
    v <- evalExpr (e' `withLocOf` e)

    insertSymbol Global name (Some ty)
    insertConstant name (SomeExpr (T.Literal v ty) ty)

    let c' = T.Constant (Loc name rgn) msTy' (SomeExpr e' ty `withLocOf` e)
    modifying constantDefs (c' :)


addFunctionSignature :: UFunction -> Builder ()
addFunctionSignature (U.Function (Loc name _) params sTy _) = do
    paramTys <- traverse (fmap snd . fromSyntaxType . U.paramType) params
    tyResult <- snd <$> fromSyntaxType sTy
    insertSymbol Global name (foldr mkTyFunc tyResult paramTys)
  where
    mkTyFunc (Some a) (Some b) = Some (a --> b)


addFunction :: UFunction -> Builder ()
addFunction (U.Function (Loc name _) params sTy body) = do
    paramSyms <- traverse paramToSym params
    tyResult  <- snd <$> fromSyntaxType sTy

    f <- runTypeChecker (tcFunctionDef paramSyms tyResult body)
    insertConstant name f
  where
    paramToSym (U.Parameter n psTy) = do
        ty <- snd <$> fromSyntaxType psTy
        return (unLoc n, ty)


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
            lower' <- fst <$> evalIntExpr lower
            upper' <- fst <$> evalIntExpr upper
            checkCardinalities lower upper lower' upper'
            return (RoleRef tyName (lower', upper'))

    checkCardinalities lower upper lower' upper'
        | lower' < 0 = throw (getLoc lower) (InvalidLowerBound lower')
        | upper' < lower' =
            throw
                (getLoc lower <> getLoc upper)
                (InvalidCardinalities lower' upper')
        | otherwise = return ()


addComponents :: ComponentDef -> Builder ()
addComponents (ComponentDef (Loc name _) (Loc tyName _) mLen) = case mLen of
    -- add component array
    Just len -> do
        len' <- fst <$> evalIntExpr len
        if len' > 0
            then
                let tyArray = TyArray len' tyComponent
                in insertSymbol Global name (Some tyArray)
            else throw (getLoc len) (InvalidUpperBound len')
    -- add single component
    Nothing -> insertSymbol Global name (Some tyComponent)
  where
    tyComponent = TyComponent (Set.singleton tyName)


fromSyntaxType :: UType -> Builder (TType, Some Type)
fromSyntaxType = \case
    U.TyBool   -> return (T.TyBool  , Some TyBool)
    U.TyInt    -> return (T.TyInt   , Some TyInt)
    U.TyDouble -> return (T.TyDouble, Some TyDouble)
    U.TyAction -> return (T.TyAction, Some TyAction)
    U.TyComponent tySet -> do
        compTys <- use (modelInfo.componentTypes)
        tySet' <- lift (fromEither' (normalizeTypeSet compTys tySet))
        return (T.TyComponent tySet, Some (TyComponent tySet'))
    U.TyArray size sTy -> do
        (sizeVal, size') <- evalIntExpr size
        (sTy', Some ty) <- fromSyntaxType sTy
        return
            ( T.TyArray size' sTy'
            , Some (TyArray sizeVal ty)
            )
    U.TyFunc sTyL sTyR -> do
        (sTyL', Some tyL) <- fromSyntaxType sTyL
        (sTyR', Some tyR) <- fromSyntaxType sTyR
        return (T.TyFunc sTyL' sTyR', Some (tyL --> tyR))


fromSyntaxVarType :: UVarType -> Builder (Some Type, Maybe (Int, Int))
fromSyntaxVarType = \case
    U.VarTyBool ->
        return (Some TyBool, Nothing)
    U.VarTyInt (lower, upper) -> do
        (lowerVal, _) <- evalIntExpr lower
        (upperVal, _) <- evalIntExpr upper
        return (Some TyInt, Just (lowerVal, upperVal))
    U.VarTyEnum (Enumeration names) ->
        return (Some TyInt, Just (0, length names - 1))
    U.VarTyArray size vTy -> do
        (sizeVal, _) <- evalIntExpr size
        (Some ty, mRange) <- fromSyntaxVarType vTy
        return (Some (TyArray sizeVal ty), mRange)


type Builder a = StateT BuilderState Result a


runBuilder :: Builder a -> RecursionDepth -> Result (ModelInfo, [TConstant])
runBuilder m depth = do
    BuilderState mi defs _ <- execStateT m initial
    return (mi, defs)
  where
    initial = BuilderState emptyModelInfo [] depth


evalIntExpr :: Loc U.Expr -> Builder (Int, Loc SomeExpr)
evalIntExpr e = do
    e' <- typeCheckExpr TyInt e
    v  <- evalExpr (e' `withLocOf` e)
    return (v, SomeExpr e' TyInt `withLocOf` e)


evalExpr :: Loc (T.Expr t) -> Builder t
evalExpr e = do
    env <- get
    runReaderT (eval e) env


typeCheckExpr :: Type t -> Loc U.Expr -> Builder (T.Expr t)
typeCheckExpr ty e = runTypeChecker (e `hasType` ty)


runTypeChecker :: TC.TypeChecker a -> Builder a
runTypeChecker m = do
    compTys  <- use (modelInfo.componentTypes)
    symTable <- use (modelInfo.symbolTable)
    consts   <- use (modelInfo.constants)
    depth    <- use recursionDepth
    lift (TC.runTypeChecker m compTys symTable consts depth)


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
