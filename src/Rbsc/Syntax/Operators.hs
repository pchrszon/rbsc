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
    , logicOpShortcut

    , Quantifier(..)
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


-- | Shortcut-semanitcs of a 'LogicOp'. If the left-hand operand already
-- fully determines the truth-value of the operator, then @Just@ the
-- truth-value is returned. If the second operand must be evaluated as
-- well, then @Nothing@ is returned.
logicOpShortcut :: LogicOp -> Bool -> Maybe Bool
logicOpShortcut binOp l = case binOp of
    And | not l     -> Just False
    Or  | l         -> Just True
    Implies | not l -> Just True
    _ -> Nothing


-- | A quantifier.
data Quantifier
    = Forall
    | Exists
    deriving (Eq, Show)
