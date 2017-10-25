{-# LANGUAGE LambdaCase #-}


-- | Operators and quantifiers.
module Rbsc.Syntax.Operators
    ( ArithOp(..)
    , arithOp

    , EqOp(..)
    , eqOp

    , RelOp(..)
    , relOp

    , LogicOp(..)
    , logicOp

    , Quantifier(..)
    , quantifier
    ) where


-- | Arithmetic operators.
data ArithOp
    = Add
    | Sub
    | Mul
    deriving (Eq, Show)


-- | Semantics of an 'ArithOp'.
arithOp :: Num a => ArithOp -> a -> a -> a
arithOp = \case
    Add -> (+)
    Sub -> (-)
    Mul -> (*)


-- | Equality operators.
data EqOp
    = Eq
    | NEq
    deriving (Eq, Show)


-- | Semantics of an 'EqOp'.
eqOp :: Eq a => EqOp -> a -> a -> Bool
eqOp = \case
    Eq  -> (==)
    NEq -> (/=)


-- | Relation operators.
data RelOp
    = Lt
    | Lte
    | Gt
    | Gte
    deriving (Eq, Show)


-- | Semantics of a 'RelOp'.
relOp :: Ord a => RelOp -> a -> a -> Bool
relOp = \case
    Lt  -> (<)
    Lte -> (<=)
    Gt  -> (>)
    Gte -> (>=)


-- | Boolean logic operators.
data LogicOp
    = And
    | Or
    | Implies
    deriving (Eq, Show)


-- | Semantics of a 'LogicOp'.
logicOp :: LogicOp -> Bool -> Bool -> Bool
logicOp binOp l r = case binOp of
    And     -> l && r
    Or      -> l || r
    Implies -> not l || r


-- | A quantifier.
data Quantifier
    = Forall
    | Exists
    deriving (Eq, Show)


-- | Semantics of a 'Quantifier'.
quantifier :: Foldable t => Quantifier -> t Bool -> Bool
quantifier = \case
    Forall -> and
    Exists -> or
