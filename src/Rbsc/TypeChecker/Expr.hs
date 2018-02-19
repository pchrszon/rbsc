{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}


-- | Type checking of expressions.
module Rbsc.TypeChecker.Expr
    ( tcExpr
    , hasType
    ) where


import Control.Lens
import Control.Monad.Reader

import Data.List.NonEmpty (NonEmpty (..), fromList)


import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..), Region)

import           Rbsc.Syntax.Expr.Typed   (SomeExpr (..))
import qualified Rbsc.Syntax.Expr.Typed   as T
import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker.Internal


-- | Type check an untyped expression. If the expression is well-typed,
-- a typed expression is returned, together with a type-level witness.
tcExpr :: Loc U.Expr -> TypeChecker SomeExpr
tcExpr (Loc e rgn) = case e of
    U.LitBool b ->
        T.Literal b `withType` TyBool

    U.LitInt i ->
        T.Literal i `withType` TyInt

    U.LitDouble d ->
        T.Literal d `withType` TyDouble

    U.LitFunction f ->
        return (fromFunctionName f)

    U.LitArray es ->
        tcArray es

    U.Identifier name ->
        lookupBoundVar name >>= \case
            Just (i, SomeType ty) -> do
                Refl <- expect tyComponent rgn ty
                T.Bound i ty `withType` ty
            Nothing -> do
                SomeType ty <- getIdentifierType name rgn
                T.Identifier name ty `withType` ty

    U.Not inner -> do
        inner' <- inner `hasType` TyBool
        T.Not inner' `withType` TyBool

    U.Negate inner -> do
        SomeExpr inner' ty <- tcExpr inner
        Dict <- isNumType ty (getLoc inner)
        T.Negate inner' `withType` ty

    U.ArithOp aOp l r -> do
        (SomeExpr l' tyL, SomeExpr r' tyR) <-
            binaryCast <$> tcExpr l <*> tcExpr r
        Dict <- isNumType tyL (getLoc l)
        _    <- isNumType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.ArithOp aOp l' r' `withType` tyL

    U.Divide l r -> do
        l' <- l `hasType` TyDouble
        r' <- r `hasType` TyDouble
        T.Divide rgn l' r' `withType` TyDouble

    U.EqOp eOp l r -> do
        (SomeExpr l' tyL, SomeExpr r' tyR) <-
            binaryCast <$> tcExpr l <*> tcExpr r
        Dict <- isEqType tyL (getLoc l)
        _    <- isEqType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.EqOp eOp l' r' `withType` TyBool

    U.RelOp rOp l r -> do
        (SomeExpr l' tyL, SomeExpr r' tyR) <-
            binaryCast <$> tcExpr l <*> tcExpr r
        Dict <- isOrdType tyL (getLoc l)
        _    <- isOrdType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.RelOp rOp l' r' `withType` TyBool

    U.LogicOp lOp l r -> do
        l' <- l `hasType` TyBool
        r' <- r `hasType` TyBool
        T.LogicOp lOp l' r' `withType` TyBool

    U.Index inner idx -> do
        SomeExpr inner' ty <- tcExpr inner
        case ty of
            TyArray _ elemTy -> do
                idx' <- idx `hasType` TyInt
                Dict <- return (dictShow elemTy)
                T.Index inner' (Loc idx' (getLoc idx)) `withType` elemTy
            _ -> throw (getLoc inner) (NotAnArray (renderType ty))

    U.Call f args -> do
        f' <- tcExpr f
        checkCallArity rgn f' args
        tcCall (Loc f' (getLoc f)) args

    U.HasType inner tyName ->
        whenTypeExists tyName $ do
            inner' <- inner `hasType` tyComponent
            T.HasType inner' (unLoc tyName) `withType` TyBool

    U.BoundTo l r -> do
        l' <- l `hasType` tyComponent
        r' <- r `hasType` tyComponent
        T.BoundTo l' r' `withType` TyBool

    U.Element l r -> do
        l' <- l `hasType` tyComponent
        r' <- r `hasType` tyComponent
        T.Element l' r' `withType` TyBool

    U.Quantified q varName mTyName body -> do
        varTy <- case mTyName of
            Just tyName -> whenTypeExists tyName $
                return (TyComponent (Just (unLoc tyName)))
            Nothing -> return (TyComponent Nothing)

        body' <- local (over boundVars ((varName, SomeType varTy) :)) $
            body `hasType` TyBool

        T.Quantified q (fmap unLoc mTyName) (T.Scope body') `withType` TyBool


fromFunctionName :: FunctionName -> SomeExpr
fromFunctionName = \case
    FuncMinInt ->
        SomeExpr (T.LitFunction MinInt) (TyInt --> TyInt --> TyInt)
    FuncMinDouble ->
        SomeExpr (T.LitFunction MinDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMaxInt ->
        SomeExpr (T.LitFunction MaxInt) (TyInt --> TyInt --> TyInt)
    FuncMaxDouble ->
        SomeExpr (T.LitFunction MaxDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncFloor ->
        SomeExpr (T.LitFunction Floor) (TyDouble --> TyInt)
    FuncCeil ->
        SomeExpr (T.LitFunction Ceil) (TyDouble --> TyInt)
    FuncPowInt ->
        SomeExpr (T.LitFunction PowInt) (TyInt --> TyInt --> TyInt)
    FuncPowDouble ->
        SomeExpr (T.LitFunction PowDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMod ->
        SomeExpr (T.LitFunction Mod) (TyInt --> TyInt --> TyInt)
    FuncLog ->
        SomeExpr (T.LitFunction Log) (TyDouble --> TyDouble --> TyDouble)


tcArray :: NonEmpty (Loc U.Expr) -> TypeChecker SomeExpr
tcArray (e :| es) = do
    SomeExpr e' ty <- tcExpr e
    Dict <- pure (dictShow ty)
    es' <- traverse (`hasType` ty) es
    return (SomeExpr (T.LitArray (e' :| es')) (TyArray (0, length es) ty))


checkCallArity :: Region -> SomeExpr -> [args] -> TypeChecker ()
checkCallArity rgn (SomeExpr _ ty) args
    | numParams > 0 && numParams < numArgs =
        throw rgn (WrongNumberOfArguments numParams numArgs)
    | otherwise = return ()
  where
    numParams = paramsLength ty
    numArgs   = length args

    paramsLength :: Type t -> Int
    paramsLength = \case
        TyFunc _ tyRes -> 1 + paramsLength tyRes
        _              -> 0


tcCall :: Loc SomeExpr -> [Loc U.Expr] -> TypeChecker SomeExpr
tcCall (Loc (SomeExpr e ty) _) [] = return (SomeExpr e ty)
tcCall (Loc (SomeExpr f (TyFunc tyParam tyRes)) rgn) (arg : args) = do
    arg' <- arg `hasType` tyParam
    Dict <- return (dictShow tyRes)
    tcCall (Loc (SomeExpr (T.Apply f arg') tyRes) rgn) args
tcCall (Loc (SomeExpr _ ty) rgn) (_ : _) =
    throw rgn (NotAFunction (renderType ty))


-- | Assume that a given untyped expression has a given 'Type'.
--
-- If the expression has not the given type, but a dynamic cast is
-- possible, then the dynamic cast will be performed.
hasType :: Loc U.Expr -> Type t -> TypeChecker (T.Expr t)
hasType e expected = do
    SomeExpr e' actual <- cast expected <$> tcExpr e
    Refl <- expect expected (getLoc e) actual
    return e'


-- | If one of the two given expression has 'TyDouble' and the other one
-- has 'TyInt', then cast the @TyInt@ expression to @TyDouble@.
binaryCast :: SomeExpr -> SomeExpr -> (SomeExpr, SomeExpr)
binaryCast (SomeExpr l TyDouble) (SomeExpr r TyInt) =
    (SomeExpr l TyDouble, SomeExpr (T.Cast r) TyDouble)
binaryCast (SomeExpr l TyInt) (SomeExpr r TyDouble) =
    (SomeExpr (T.Cast l) TyDouble, SomeExpr r TyDouble)
binaryCast l r = (l, r)


-- | @cast expected e@ inserts 'Cast's if a dynamic cast to type @expected@
-- is possible. Otherwise, the original expression is returned.
cast :: Type t -> SomeExpr -> SomeExpr
cast TyDouble (SomeExpr e TyInt) = SomeExpr (T.Cast e) TyDouble
cast (TyArray tIndices TyDouble) (SomeExpr (T.LitArray es) (TyArray vIndices TyInt))
    | arrayLength tIndices == arrayLength vIndices =
        SomeExpr (T.LitArray (fmap T.Cast es)) (TyArray vIndices TyDouble)
cast arrTy@(TyArray tIndices ty) e@(SomeExpr e' elemTy) =
    case typeEq ty elemTy of
        Just Refl -> case dictShow ty of
            Dict -> SomeExpr
                (T.LitArray (fromList (replicate (arrayLength tIndices) e')))
                arrTy
        Nothing   -> e
cast _ e = e


tyComponent :: Type Component
tyComponent = TyComponent Nothing
