{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}


-- | Translation of expressions.
module Rbsc.Translator.Expr
    ( trnsLSomeExpr
    ) where


import Control.Lens

import Data.Text (pack)
import Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.Array (Array)
import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Internal


trnsLSomeExpr :: MonadEval r m => Maybe Name -> LSomeExpr -> m Prism.Expr
trnsLSomeExpr mCompName (Loc (SomeExpr e _) rgn) = trnsExpr mCompName rgn e


trnsExpr :: MonadEval r m => Maybe Name -> Region -> Expr t -> m Prism.Expr
trnsExpr mCompName rgn = go
  where
    go :: MonadEval r m => Expr t -> m Prism.Expr
    go = \case
        (trnsIdent -> Just qname) ->
            Prism.Ident <$> trnsQualified qname

        Literal b TyBool ->
            return (Prism.LitBool b)

        Literal i TyInt ->
            return (Prism.LitInt i)

        Literal d TyDouble ->
            return (Prism.LitDouble d)

        Cast inner ->
            go inner

        Not inner ->
            Prism.UnaryOp Prism.Not <$> go inner

        Negate inner ->
            Prism.UnaryOp Prism.Neg <$> go inner

        ArithOp aOp l r ->
            Prism.BinaryOp <$> go l <*> pure (trnsArithOp aOp) <*> go r

        Divide _ l r ->
            Prism.BinaryOp <$> go l <*> pure Prism.Divide <*> go r

        EqOp eOp ty l r ->
            trnsEq mCompName rgn eOp ty l r

        RelOp rOp l r ->
            Prism.BinaryOp <$> go l <*> pure (trnsRelOp rOp) <*> go r

        LogicOp lOp l r ->
            Prism.BinaryOp <$> go l <*> pure (trnsLogicOp lOp) <*> go r

        Apply f arg ->
            trnsApply f [Some arg]

        IfThenElse c t e' ->
            Prism.Ite <$> go c <*> go t <*> go e'

        e' ->
            throw rgn (TranslationNotSupported (pack (show e')))

    trnsIdent :: Expr t -> Maybe Qualified
    trnsIdent = \case
        Identifier name _ -> Just $ case mCompName of
            Just compName' -> QlMember (QlName compName') name
            Nothing        -> QlName name

        Member (Literal comp (TyComponent _)) name _ ->
            Just (QlMember (QlName (view compName comp)) name)

        Index (trnsIdent -> Just qname) (Loc (Literal i _) _) ->
            Just (QlIndex qname (fromInteger i))

        _ -> Nothing


    trnsApply :: MonadEval r m => Expr t -> [Some Expr] -> m Prism.Expr
    trnsApply f args = case f of
        LitFunction l ->
            Prism.Func (trnsFunction l) <$> traverse (\(Some e) -> go e) args
        Apply f' arg -> trnsApply f' (Some arg : args)
        _ -> throw rgn (TranslationNotSupported (pack (show f)))


trnsEq ::
       MonadEval r m
    => Maybe Name
    -> Region
    -> EqOp
    -> Type t
    -> Expr t
    -> Expr t
    -> m Prism.Expr
trnsEq mCompName rgn eOp = go
  where
    go :: MonadEval r m => Type t -> Expr t -> Expr t -> m Prism.Expr
    go ty l r = case ty of
        TyArray (lower, upper) innerTy -> do
            es' <- for [lower .. upper] $ \i ->
                    go innerTy (idx innerTy l i) (idx innerTy r i)
            return (foldr1 conn es')
        _ -> do
            Loc l' _ <- reduce (Loc l rgn)
            Loc r' _ <- reduce (Loc r rgn)
            Prism.BinaryOp
                <$> trnsExpr mCompName rgn l'
                <*> pure (trnsEqOp eOp)
                <*> trnsExpr mCompName rgn r'

    conn l r = case eOp of
               Eq  -> Prism.BinaryOp l Prism.And r
               NEq -> Prism.BinaryOp l Prism.Or r

    idx :: Type t -> Expr (Array t) -> Int -> Expr t
    idx ty e i = case dictShow ty of
        Dict -> Index e (Loc (Literal (fromIntegral i) TyInt) rgn)


trnsFunction :: TypedFunction t -> Prism.Function
trnsFunction = \case
    MinInt    -> Prism.FuncMin
    MinDouble -> Prism.FuncMin
    MaxInt    -> Prism.FuncMax
    MaxDouble -> Prism.FuncMax
    Floor     -> Prism.FuncFloor
    Ceil      -> Prism.FuncCeil
    PowInt    -> Prism.FuncPow
    PowDouble -> Prism.FuncPow
    Mod       -> Prism.FuncMod
    Log       -> Prism.FuncLog


trnsArithOp :: ArithOp -> Prism.BinaryOp
trnsArithOp = \case
    Add -> Prism.Plus
    Sub -> Prism.Minus
    Mul -> Prism.Times


trnsEqOp :: EqOp -> Prism.BinaryOp
trnsEqOp = \case
    Eq  -> Prism.Eq
    NEq -> Prism.Neq


trnsRelOp :: RelOp -> Prism.BinaryOp
trnsRelOp = \case
    Lt  -> Prism.Lt
    Lte -> Prism.Le
    Gt  -> Prism.Gt
    Gte -> Prism.Ge


trnsLogicOp :: LogicOp -> Prism.BinaryOp
trnsLogicOp = \case
    And     -> Prism.And
    Or      -> Prism.Or
    Implies -> Prism.Implies
