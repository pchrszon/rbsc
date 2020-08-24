{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Abstract syntax of expressions.
module Language.Prism.Expression
    ( Ident
    , Expr(..)
    , BoundStrictness(..)
    ) where


import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc

import Language.Prism.Functions
import Language.Prism.Operators


-- | Represents an identifier.
type Ident = Text


-- | PRISM expressions.
data Expr
    = BinaryOp Expr !BinaryOp Expr
    | UnaryOp !UnaryOp Expr
    | Ident !Ident
    | Func !Function [Expr]
    | LitBool !Bool
    | LitInt !Integer
    | LitDouble !Double
    | Ite Expr Expr Expr
    | Exists Expr
    | Forall Expr
    | Prob !RelOp (Maybe Expr) Expr -- ^ @P rel-op bound [ expr ]@
    | Reward (Maybe Ident) !RelOp (Maybe Expr) Expr -- ^ @R{"reward-struct"} rel-op bound [ expr ]@
    | Steady !RelOp (Maybe Expr) Expr -- ^ @S rel-op bound [ expr ]@
    | Temporal
        (Maybe Expr)
        !TemporalOp
        (Maybe Expr)
        (Maybe Expr)
        !BoundStrictness
        (Maybe Expr)
        !BoundStrictness -- ^ @ExprTemporal@: left operand, operator, right operand, lower time bound, lower bound strictness, upper time bound, upper bound strictness
    | LitLabel !Ident
    | Filter !FilterOp Expr Expr
    deriving (Show)

-- | Strictness of bounds in temporal operator.
data BoundStrictness = Inclusive | Exclusive deriving (Show)

instance Num Expr where
    l + r = BinaryOp l Plus r
    l - r = BinaryOp l Minus r
    l * r = BinaryOp l Times r
    negate = UnaryOp Neg
    abs e = Ite (BinaryOp e Lt 0) (negate e) e
    signum e = Ite (BinaryOp e Lt 0) (-1) (Ite (BinaryOp e Gt 0) 1 0)
    fromInteger = LitInt

instance Pretty Expr where
    pretty = go 0 where
        go outerPrec expr =
            let prec = precExpr expr
                doc  = prettyExpr (go prec) expr
            in parensIf (outerPrec > prec) doc


prettyExpr :: (Expr -> Doc ann) -> Expr -> Doc ann
prettyExpr f = \case
    BinaryOp l op r -> f l <+> pretty op <+> f r
    UnaryOp op e -> pretty op <> f e
    Ident ident -> pretty ident
    Func func es ->
        pretty func <> parens (mconcat (punctuate ", " (pretty <$> es)))
    LitBool b
        | b         -> "true"
        | otherwise -> "false"
    LitInt i -> pretty i
    LitDouble d -> pretty d
    Ite i t e -> f i <+> "?" <+> f t <+> colon <+> f e
    Exists e -> "E" <+> path e
    Forall e -> "A" <+> path e
    Prob relOp prob e -> "P" <> rel relOp prob <+> path e
    Reward s relOp reward e ->
        "R" <> struct s <> rel relOp reward <+> path e
    Steady relOp prob e -> "S" <> rel relOp prob <+> path e
    Temporal l op r lower lowerStrict upper upperStrict ->
        maybe mempty ((<> space) . f) l <>
        pretty op <> timeBound lower lowerStrict upper upperStrict <>
        maybe mempty ((space <>) . f) r
    LitLabel ident -> dquotes (pretty ident)
    Filter op e filterExpr -> "filter" <>
        parens (pretty op <> ", " <> pretty e <> ", " <> pretty filterExpr)
  where
    rel relOp bound = pretty relOp <> maybe "?" f bound
    path e = brackets (space <> pretty e <> space)
    struct = maybe mempty (braces . dquotes . pretty)
    timeBound lower lowerStrict upper upperStrict =
        case (lower, upper) of
            (Nothing, Nothing) -> mempty
            (Just l , Nothing) -> strict "<" lowerStrict l
            (Nothing, Just u)  -> strict ">" upperStrict u
            (Just l , Just u)  -> brackets (pretty l <> comma <> pretty u)
    strict op strictness e =
        let s = case strictness of
                    Inclusive -> "="
                    Exclusive -> mempty
        in op <> s <> pretty e

parensIf :: Bool -> Doc ann -> Doc ann
parensIf b doc
    | b = parens doc
    | otherwise = doc

precExpr :: Expr -> Int
precExpr = \case
    BinaryOp _ op _ -> precBinOp op
    UnaryOp op _ -> precUnOp op
    Ite {} -> 3
    Temporal _ op _ _ _ _ _ -> case op of
        Next     -> 2
        Finally  -> 2
        Globally -> 2
        _        -> 1
    _ -> 14
