{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}


-- | Abstract syntax of untyped expressions.
module Rbsc.Syntax.Expr.Untyped
    ( Expr(..)
    , LExpr

    , pattern Index'
    , pattern HasType'

    , clauses
    ) where


import Control.Lens

import Data.List.NonEmpty (NonEmpty)


import Rbsc.Data.ComponentType
import Rbsc.Data.Function
import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Operators


-- | An untyped expression.
data Expr
    = LitBool !Bool
    | LitInt !Integer
    | LitDouble !Double
    | LitFunction !FunctionName
    | LitArray (NonEmpty LExpr)
    | Identifier !Name
    | Not LExpr
    | Negate LExpr
    | ArithOp !ArithOp LExpr LExpr
    | Divide LExpr LExpr
    | EqOp !EqOp LExpr LExpr
    | RelOp !RelOp LExpr LExpr
    | LogicOp !LogicOp LExpr LExpr
    | Index LExpr LExpr
    | Call LExpr [LExpr]
    | IfThenElse LExpr LExpr LExpr
    | HasType LExpr (Loc TypeName)
    | BoundTo LExpr LExpr
    | Element LExpr LExpr
    | Quantified !Quantifier !Name ComponentTypeSet LExpr
    deriving (Show)

instance Plated LExpr where
    plate f (Loc e rgn) = fmap (`Loc` rgn) $ case e of
        LitBool b -> pure (LitBool b)
        LitInt i -> pure (LitInt i)
        LitDouble d -> pure (LitDouble d)
        LitFunction name -> pure (LitFunction name)
        LitArray es -> LitArray <$> traverse f es
        Identifier name -> pure (Identifier name)
        Not e' -> Not <$> f e'
        Negate e' -> Negate <$> f e'
        ArithOp aOp l r -> ArithOp aOp <$> f l <*> f r
        Divide l r -> Divide <$> f l <*> f r
        EqOp eOp l r -> EqOp eOp <$> f l <*> f r
        RelOp rOp l r -> RelOp rOp <$> f l <*> f r
        LogicOp lOp l r -> LogicOp lOp <$> f l <*> f r
        Index e' idx -> Index <$> f e' <*> f idx
        Call e' args -> Call <$> f e' <*> traverse f args
        IfThenElse c t e' -> IfThenElse <$> f c <*> f t <*> f e'
        HasType e' tyName -> HasType <$> f e' <*> pure tyName
        BoundTo l r -> BoundTo <$> f l <*> f r
        Element l r -> Element <$> f l <*> f r
        Quantified q varName tySet e' -> Quantified q varName tySet <$> f e'


pattern Index' :: LExpr -> LExpr -> LExpr
pattern Index' e idx <- Loc (Index e idx) _

pattern HasType' :: LExpr -> Loc TypeName -> LExpr
pattern HasType' e tyName <- Loc (HasType e tyName) _


-- | A location-annotated 'Expr'.
type LExpr = Loc Expr


clauses :: LExpr -> [LExpr]
clauses = \case
    Loc (LogicOp And l r) _ -> clauses l ++ clauses r
    Loc (LitBool True) _ -> []
    e -> [e]
