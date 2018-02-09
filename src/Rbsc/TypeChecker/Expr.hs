{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}


-- | Type checking of expressions.
module Rbsc.TypeChecker.Expr
    ( tcExpr
    , hasType
    ) where


import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import Data.List.NonEmpty (NonEmpty (..))


import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Type

import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region     (Loc (..), Region)

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

    U.Function f ->
        return (fromFunctionSym f)

    U.Array es ->
        tcArray es

    U.Variable name ->
        lookupBoundVar name >>= \case
            Just (i, SomeType ty) -> do
                Refl <- expect tyComponent rgn ty
                T.Bound i ty `withType` ty
            Nothing -> do
                SomeType ty <- getIdentifierType name rgn
                T.Variable name ty `withType` ty

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
            TyArray elemTy _ -> do
                idx' <- idx `hasType` TyInt
                Dict <- return (dictShow elemTy)
                T.Index inner' (Loc idx' (getLoc idx)) `withType` elemTy
            _ -> throwError (Type.NotAnArray (renderType ty) (getLoc inner))

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


fromFunctionSym :: FunctionSym -> SomeExpr
fromFunctionSym = \case
    FuncMinInt ->
        SomeExpr (T.Function MinInt) (TyInt --> TyInt --> TyInt)
    FuncMinDouble ->
        SomeExpr (T.Function MinDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMaxInt ->
        SomeExpr (T.Function MaxInt) (TyInt --> TyInt --> TyInt)
    FuncMaxDouble ->
        SomeExpr (T.Function MaxDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncFloor ->
        SomeExpr (T.Function Floor) (TyDouble --> TyInt)
    FuncCeil ->
        SomeExpr (T.Function Ceil) (TyDouble --> TyInt)
    FuncPowInt ->
        SomeExpr (T.Function PowInt) (TyInt --> TyInt --> TyInt)
    FuncPowDouble ->
        SomeExpr (T.Function PowDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMod ->
        SomeExpr (T.Function Mod) (TyInt --> TyInt --> TyInt)
    FuncLog ->
        SomeExpr (T.Function Log) (TyDouble --> TyDouble --> TyDouble)


tcArray :: NonEmpty (Loc U.Expr) -> TypeChecker SomeExpr
tcArray (e :| es) = do
    SomeExpr e' ty <- tcExpr e
    Dict <- pure (dictShow ty)
    es' <- traverse (`hasType` ty) es
    return (SomeExpr (T.Array (e' :| es')) (TyArray ty (Just (length es + 1))))


checkCallArity :: Region -> SomeExpr -> [args] -> TypeChecker ()
checkCallArity rgn (SomeExpr _ ty) args
    | numParams > 0 && numParams < numArgs =
        throwError (Type.WrongNumberOfArguments numParams numArgs rgn)
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
    throwError (Type.NotAFunction (renderType ty) rgn)


-- | Assume that a given untyped expression has a given 'Type'.
--
-- If the expected type is 'TyDouble' but the expression has type 'TyInt',
-- a 'Cast' is inserted automatically.
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


-- | @cast expected e@ casts expression @t@ to 'TyDouble' if @expected@ is
-- @TyDouble@ and the expression has type 'TyInt'.
cast :: Type t -> SomeExpr -> SomeExpr
cast TyDouble (SomeExpr e TyInt) = SomeExpr (T.Cast e) TyDouble
cast (TyArray TyDouble tLen) (SomeExpr (T.Array es) (TyArray TyInt vLen))
    | isLenghtCompatible tLen vLen =
        SomeExpr (T.Array (fmap T.Cast es)) (TyArray TyDouble vLen)
cast _ e = e


-- | @isLenghtCompatible expected actual@ returns @True@ if an array with
-- length @actual@ can be assigned to an array with length @expected@.
isLenghtCompatible :: Maybe Int -> Maybe Int -> Bool
isLenghtCompatible expected actual = liftA2 (==) expected actual /= Just False


tyComponent :: Type Component
tyComponent = TyComponent Nothing
