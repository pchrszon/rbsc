{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}


-- | Construction of the symbol table and evaluation of constants.
module Rbsc.TypeChecker.ModelInfo
    ( getModelInfo
    ) where


import Control.Lens
import Control.Monad.State.Strict

import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Semigroup
import qualified Data.Set        as Set


import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed   (SomeExpr (..), TConstant, TType)
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped (UConstant, UFunction, UGlobal, UModel,
                                      UType, UVarType)
import qualified Rbsc.Syntax.Untyped as U

import           Rbsc.TypeChecker.ComponentTypes
import           Rbsc.TypeChecker.Dependencies
import           Rbsc.TypeChecker.Expr
import           Rbsc.TypeChecker.Identifiers
import qualified Rbsc.TypeChecker.Internal       as TC


data BuilderState = BuilderState
    { _modelInfo      :: !ModelInfo
    , _constantDefs   :: [TConstant]
    , _recursionDepth :: RecursionDepth
    }

makeLenses ''BuilderState


-- | Construct the 'ModelInfo' for a given 'Model'. Since evaluation of
-- constants requires type checking the constant definitions, the checked
-- definitions are returned as well.
getModelInfo ::
       RecursionDepth -> UModel -> Result' (ModelInfo, [TConstant])
getModelInfo depth m = do
    idents <- fromEither (identifierDefs m)
    deps   <- fromEither' (sortDefinitions idents)
    result <- runBuilder (traverse addDependency deps) depth
    validateComponentTypes (view (_1.componentTypes) result) m
    return result


addDependency :: Dependency -> Builder ()
addDependency = \case
    DepDefinition def -> case def of
        DefConstant c      -> addConstant c
        DefFunction f      -> addFunction f
        DefGlobal g        -> addGlobal g
        DefComponentType t -> addComponentType t
        DefComponent c     -> addComponents c
    DepFunctionSignature f -> addFunctionSignature f


addConstant :: UConstant -> Builder ()
addConstant (U.Constant (Loc name rgn) msTy e) = do
    (msTy', SomeExpr e' ty) <- case msTy of
        Just sTy -> do
            -- if an explicit type annotation is given, check if type of
            -- the definition matches
            (sTy', SomeType ty) <- fromSyntaxType sTy
            e' <- typeCheckExpr ty e
            return (Just sTy', SomeExpr e' ty)
        Nothing -> do
            e' <- runTypeChecker (tcExpr e)
            return (Nothing, e')

    Dict <- return (dictShow ty)
    v <- evalExpr (e' `withLocOf` e)

    insertSymbol name (SomeType ty)
    insertConstant name (SomeExpr (T.Literal v) ty)

    let c' = T.Constant (Loc name rgn) msTy' (SomeExpr e' ty `withLocOf` e)
    modifying constantDefs (c' :)


addFunctionSignature :: UFunction -> Builder ()
addFunctionSignature (U.Function (Loc name _) params sTy _) = do
    paramTys <- traverse (fmap snd . fromSyntaxType . U.paramType) params
    tyResult <- snd <$> fromSyntaxType sTy
    insertSymbol name (foldr mkTyFunc tyResult paramTys)
  where
    mkTyFunc (SomeType a) (SomeType b) = SomeType (a --> b)


addFunction :: UFunction -> Builder ()
addFunction (U.Function (Loc name _) params sTy body) = do
    paramSyms <- toList <$> traverse paramToSym params
    tyResult <- snd <$> fromSyntaxType sTy

    f <- runTypeChecker (tcFunctionDef paramSyms tyResult body)
    insertConstant name f
  where
    paramToSym (U.Parameter n psTy) = do
        ty <- snd <$> fromSyntaxType psTy
        return (unLoc n, ty)


addGlobal :: UGlobal -> Builder ()
addGlobal (U.Global (Loc name _) vTy _) = do
    ty <- fromSyntaxVarType vTy
    insertSymbol name ty


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
            lower' <- fst <$> evalIntegerExpr lower
            upper' <- fst <$> evalIntegerExpr upper
            checkCardinalities lower upper lower' upper'
            return (RoleRef tyName (lower', upper'))

    checkCardinalities lower upper lower' upper'
        | lower' < 0 = throwOne (getLoc lower) (InvalidLowerBound lower')
        | upper' < lower' =
            throwOne
                (getLoc lower <> getLoc upper)
                (InvalidCardinalities lower' upper')
        | otherwise = return ()


addComponents :: ComponentDef -> Builder ()
addComponents (ComponentDef (Loc name _) (Loc tyName _) mLen) = case mLen of
    -- add component array
    Just len -> do
        len' <- fst <$> evalIntegerExpr len
        if len' > 0
            then
                let tyArray = TyArray (0, len' - 1) tyComponent
                in insertSymbol name (SomeType tyArray)
            else throwOne (getLoc len) (InvalidUpperBound len')
    -- add single component
    Nothing ->
        insertSymbol name (SomeType tyComponent)
  where
    tyComponent = TyComponent (Set.singleton tyName)


fromSyntaxType :: UType -> Builder (TType, SomeType)
fromSyntaxType = \case
    U.TyBool   -> return (T.TyBool  , SomeType TyBool)
    U.TyInt    -> return (T.TyInt   , SomeType TyInt)
    U.TyDouble -> return (T.TyDouble, SomeType TyDouble)
    U.TyComponent tySet -> do
        compTys <- use (modelInfo.componentTypes)
        tySet' <- lift (fromEither' (normalizeTypeSet compTys tySet))
        return (T.TyComponent tySet, SomeType (TyComponent tySet'))
    U.TyArray (lower, upper) sTy -> do
        (lowerVal, lower') <- evalIntegerExpr lower
        (upperVal, upper') <- evalIntegerExpr upper
        (sTy', SomeType ty) <- fromSyntaxType sTy
        return
            ( T.TyArray (lower', upper') sTy'
            , SomeType (TyArray (lowerVal, upperVal) ty)
            )
    U.TyFunc sTyL sTyR -> do
        (sTyL', SomeType tyL) <- fromSyntaxType sTyL
        (sTyR', SomeType tyR) <- fromSyntaxType sTyR
        return (T.TyFunc sTyL' sTyR', SomeType (tyL --> tyR))


fromSyntaxVarType :: UVarType -> Builder SomeType
fromSyntaxVarType = \case
    U.VarTyBool  -> return (SomeType TyBool)
    U.VarTyInt _ -> return (SomeType TyInt)
    U.VarTyArray (lower, upper) vTy -> do
        (lowerVal, _) <- evalIntegerExpr lower
        (upperVal, _) <- evalIntegerExpr upper
        SomeType ty <- fromSyntaxVarType vTy
        return (SomeType (TyArray (lowerVal, upperVal) ty))


type Builder a = StateT BuilderState (Result Errors) a


runBuilder :: Builder a -> RecursionDepth -> Result' (ModelInfo, [TConstant])
runBuilder m depth = do
    BuilderState mi defs _ <- execStateT m initial
    return (mi, defs)
  where
    initial = BuilderState (ModelInfo Map.empty Map.empty Map.empty) [] depth


evalIntegerExpr :: Num a => Loc U.Expr -> Builder (a, Loc SomeExpr)
evalIntegerExpr e = do
    e' <- typeCheckExpr TyInt e
    v  <- fromInteger <$> evalExpr (e' `withLocOf` e)
    return (v, SomeExpr e' TyInt `withLocOf` e)


evalExpr :: Loc (T.Expr t) -> Builder t
evalExpr e = do
    consts <- use (modelInfo.constants)
    depth  <- use recursionDepth
    lift (fromEither' (eval consts depth e))


typeCheckExpr :: Type t -> Loc U.Expr -> Builder (T.Expr t)
typeCheckExpr ty e = runTypeChecker (e `hasType` ty)


runTypeChecker :: TC.TypeChecker a -> Builder a
runTypeChecker m = do
    compTys  <- use (modelInfo.componentTypes)
    symTable <- use (modelInfo.symbolTable)
    lift (TC.runTypeChecker m compTys symTable)


insertSymbol :: Name -> SomeType -> Builder ()
insertSymbol name ty = modelInfo.symbolTable.at name .= Just ty


insertConstant :: Name -> SomeExpr -> Builder ()
insertConstant name e = modelInfo.constants.at name .= Just e


insertComponentType :: TypeName -> ComponentType -> Builder ()
insertComponentType tyName ty = modelInfo.componentTypes.at tyName .= Just ty
