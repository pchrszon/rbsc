{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Translation of expressions.
module Rbsc.Translator.Expr
    ( trnsLSomeExpr
    ) where


import Control.Lens

import Data.Text        (pack)
import Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.Array     (Array)
import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Internal


trnsLSomeExpr
    :: Maybe (TypeName, ComponentName) -> LSomeExpr -> Translator Prism.Expr
trnsLSomeExpr mComp (Loc (SomeExpr e _) rgn) = trnsExpr mComp rgn e


trnsExpr
    :: Maybe (TypeName, ComponentName)
    -> Region
    -> Expr t
    -> Translator Prism.Expr
trnsExpr mComp rgn = go
  where
    go :: Expr t -> Translator Prism.Expr
    go e = do
        symTable <- view symbolTable
        case e of
            (trnsIdent symTable -> Just qname) ->
                Prism.Ident <$> trnsQualified qname

            Literal b TyBool ->
                return (Prism.LitBool b)

            Literal i TyInt ->
                return (Prism.LitInt (fromIntegral i))

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
                trnsEq mComp rgn eOp ty l r

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

    trnsIdent :: SymbolTable -> Expr t -> Maybe Qualified
    trnsIdent symTable = \case
        Identifier name _ -> case mComp of
            Just (typeName, compName') | isLocalSymbol symTable typeName name ->
                Just (QlMember (QlName (trnsComponentName compName')) name)
            _ -> Just (QlName name)

        Member (Literal comp (TyComponent _)) name _ ->
            let name' = trnsComponentName (view compName comp)
            in Just (QlMember (QlName name') name)

        Index (trnsIdent symTable -> Just qname) (Loc (Literal i _) _) ->
            Just (QlIndex qname i)

        _ -> Nothing


    trnsApply :: Expr t -> [Some Expr] -> Translator Prism.Expr
    trnsApply f args = case f of
        LitFunction l ->
            Prism.Func (trnsFunction l) <$> traverse (\(Some e) -> go e) args
        Apply f' arg -> trnsApply f' (Some arg : args)
        _ -> throw rgn (TranslationNotSupported (pack (show f)))


trnsEq
    :: Maybe (TypeName, ComponentName)
    -> Region
    -> EqOp
    -> Type t
    -> Expr t
    -> Expr t
    -> Translator Prism.Expr
trnsEq mComp rgn eOp = go
  where
    go :: Type t -> Expr t -> Expr t -> Translator Prism.Expr
    go ty l r = case ty of
        TyArray (lower, upper) innerTy -> do
            es' <- for [lower .. upper] $ \i ->
                    go innerTy (idx innerTy l i) (idx innerTy r i)
            return (foldr1 conn es')
        _ -> do
            Loc l' _ <- reduce (Loc l rgn)
            Loc r' _ <- reduce (Loc r rgn)
            Prism.BinaryOp
                <$> trnsExpr mComp rgn l'
                <*> pure (trnsEqOp eOp)
                <*> trnsExpr mComp rgn r'

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
