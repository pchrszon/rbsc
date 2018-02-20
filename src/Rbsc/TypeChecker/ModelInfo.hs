{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}


-- | Construction of the symbol table and evaluation of constants.
module Rbsc.TypeChecker.ModelInfo
    ( getModelInfo
    ) where


import Control.Lens
import Control.Monad.State.Strict

import           Data.Foldable
import qualified Data.Map.Strict as Map


import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import           Rbsc.Syntax.Typed   (SomeExpr (..), TConstant, TType)
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped (UConstant, UFunction, UModel, UType)
import qualified Rbsc.Syntax.Untyped as U

import Rbsc.TypeChecker.ComponentTypes
import Rbsc.TypeChecker.Dependencies
import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Identifiers
import qualified Rbsc.TypeChecker.Internal as TC


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
       RecursionDepth -> UModel -> Either [Error] (ModelInfo, [TConstant])
getModelInfo depth m = do
    types  <- getComponentTypes m
    idents <- identifierDefs m
    deps   <- toErrorList (sortDefinitions idents)
    toErrorList (runBuilder (traverse addDependency deps) depth types)
  where
    toErrorList = over _Left (: [])


addDependency :: Dependency -> Builder ()
addDependency = \case
    DepConstant c            -> addConstant c
    DepFunctionSignature f   -> addFunctionSignature f
    DepFunction f            -> addFunction f
    DepComponent name tyName -> addComponent name tyName


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
    resultTy <- snd <$> fromSyntaxType sTy
    insertSymbol name (foldr mkTyFunc resultTy paramTys)
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


addComponent :: Name -> Loc TypeName -> Builder ()
addComponent name (Loc tyName rgn) = do
    types <- use (modelInfo.componentTypes)
    if tyName `Map.member` types
        then insertSymbol name (SomeType (TyComponent (Just tyName)))
        else throw rgn UndefinedType


fromSyntaxType :: UType -> Builder (TType, SomeType)
fromSyntaxType = \case
    U.TyBool   -> return (T.TyBool  , SomeType TyBool)
    U.TyInt    -> return (T.TyInt   , SomeType TyInt)
    U.TyDouble -> return (T.TyDouble, SomeType TyDouble)
    U.TyArray (lower, upper) sTy -> do
        (lowerVal, lower') <- evalIntegerExpr lower
        (upperVal, upper') <- evalIntegerExpr upper
        (sTy', SomeType ty) <- fromSyntaxType sTy
        return
            ( T.TyArray (lower', upper') sTy'
            , SomeType (TyArray (lowerVal, upperVal) ty)
            )

type Builder a = StateT BuilderState (Either Error) a


runBuilder ::
       Builder a
    -> RecursionDepth
    -> ComponentTypes
    -> Either Error (ModelInfo, [TConstant])
runBuilder m depth types = do
    BuilderState mi defs _ <- execStateT m initial
    return (mi, defs)
  where
    initial = BuilderState (ModelInfo types Map.empty Map.empty) [] depth


evalIntegerExpr :: Num a => Loc U.Expr -> Builder (a, Loc SomeExpr)
evalIntegerExpr e = do
    e' <- typeCheckExpr TyInt e
    v  <- fromInteger <$> evalExpr (e' `withLocOf` e)
    return (v, SomeExpr e' TyInt `withLocOf` e)



evalExpr :: Loc (T.Expr t) -> Builder t
evalExpr e = do
    consts <- use (modelInfo.constants)
    depth  <- use recursionDepth
    lift (eval consts depth e)


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
