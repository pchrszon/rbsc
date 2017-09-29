{-# LANGUAGE LambdaCase #-}


-- | Operators and quantifiers.
module Rbsc.Syntax.Operators
    ( BoolBinOp(..)
    , boolBinOp
    , Quantifier(..)
    , quantifier
    ) where


-- | Boolean binary operators.
data BoolBinOp
    = And
    | Or
    | Implies
    deriving (Show)


-- | Semantics of a 'BoolBinOp'.
boolBinOp :: BoolBinOp -> Bool -> Bool -> Bool
boolBinOp binOp l r = case binOp of
    And     -> l && r
    Or      -> l || r
    Implies -> not l || r


-- | A quantifier.
data Quantifier
    = Forall
    | Exists
    deriving (Show)


-- | Semantics of a 'Quantifier'.
quantifier :: Foldable t => Quantifier -> t Bool -> Bool
quantifier = \case
    Forall -> and
    Exists -> or
