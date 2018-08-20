{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}


-- | Abstract syntax of untyped expressions.
module Rbsc.Syntax.Untyped.Expr
    ( Expr(..)
    , Quantifier(..)
    , LExpr

    , pattern Index'
    , pattern HasType'
    ) where


import Control.Lens

import Data.List.NonEmpty (NonEmpty)


import Rbsc.Data.ComponentType
import Rbsc.Data.Function
import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Operators
import Rbsc.Syntax.Quantification


-- | An untyped expression.
data Expr
    = LitBool !Bool
    | LitInt !Int
    | LitDouble !Double
    | LitAction LExpr
    | LitFunction !FunctionName
    | LitArray (NonEmpty LExpr)
    | Self
    | Player
    | ArrayIndex
    | Identifier !Name
    | Not LExpr
    | Negate LExpr
    | ArithOp !ArithOp LExpr LExpr
    | Divide LExpr LExpr
    | EqOp !EqOp LExpr LExpr
    | RelOp !RelOp LExpr LExpr
    | LogicOp !LogicOp LExpr LExpr
    | Member LExpr !Name
    | Index LExpr LExpr
    | Call LExpr [LExpr]
    | IfThenElse LExpr LExpr LExpr
    | HasType LExpr (Loc TypeName)
    | BoundTo LExpr LExpr
    | Element LExpr LExpr
    | Count ComponentTypeSet LExpr
    | Length LExpr
    | Quantified !Quantifier !Name (QuantifiedType ComponentTypeSet LExpr) LExpr
    deriving (Show)


-- | A quantifier.
data Quantifier
    = Forall
    | Exists
    | Sum
    | Product
    deriving (Show)


-- | A location-annotated 'Expr'.
type LExpr = Loc Expr

instance Plated LExpr where
    plate f (Loc e rgn) = fmap (`Loc` rgn) $ case e of
        LitBool b -> pure (LitBool b)
        LitInt i -> pure (LitInt i)
        LitDouble d -> pure (LitDouble d)
        LitAction e' -> LitAction <$> f e'
        LitFunction name -> pure (LitFunction name)
        LitArray es -> LitArray <$> traverse f es
        Self -> pure Self
        Player -> pure Player
        ArrayIndex -> pure ArrayIndex
        Identifier name -> pure (Identifier name)
        Not e' -> Not <$> f e'
        Negate e' -> Negate <$> f e'
        ArithOp aOp l r -> ArithOp aOp <$> f l <*> f r
        Divide l r -> Divide <$> f l <*> f r
        EqOp eOp l r -> EqOp eOp <$> f l <*> f r
        RelOp rOp l r -> RelOp rOp <$> f l <*> f r
        LogicOp lOp l r -> LogicOp lOp <$> f l <*> f r
        Member e' name -> Member <$> f e' <*> pure name
        Index e' idx -> Index <$> f e' <*> f idx
        Call e' args -> Call <$> f e' <*> traverse f args
        IfThenElse c t e' -> IfThenElse <$> f c <*> f t <*> f e'
        HasType e' tyName -> HasType <$> f e' <*> pure tyName
        BoundTo l r -> BoundTo <$> f l <*> f r
        Element l r -> Element <$> f l <*> f r
        Count tyName e' -> Count tyName <$> f e'
        Length e' -> Length <$> f e'
        Quantified q varName var e' ->
            Quantified q varName <$> traverse f var <*> f e'


pattern Index' :: LExpr -> LExpr -> LExpr
pattern Index' e idx <- Loc (Index e idx) _

pattern HasType' :: LExpr -> Loc TypeName -> LExpr
pattern HasType' e tyName <- Loc (HasType e tyName) _
