{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Operators provided by the PRISM language.
module Language.Prism.Operators
    ( BinaryOp(..)
    , precBinOp

    , UnaryOp(..)
    , precUnOp

    , TemporalOp(..)
    , RelOp(..)

    , FilterOp(..)
    ) where


import Data.Text.Prettyprint.Doc


-- | Binary operators.
data BinaryOp
    = Implies
    | Iff -- ^ equivalence operator
    | Or
    | And
    | Eq
    | Neq
    | Gt
    | Ge
    | Lt
    | Le
    | Plus
    | Minus
    | Times
    | Divide
    deriving (Show)

instance Pretty BinaryOp where
    pretty = \case
        Implies -> "=>"
        Iff     -> "<=>"
        Or      -> "|"
        And     -> "&"
        Eq      -> "="
        Neq     -> "!="
        Gt      -> ">"
        Ge      -> ">="
        Lt      -> "<"
        Le      -> "<="
        Plus    -> "+"
        Minus   -> "-"
        Times   -> "*"
        Divide  -> "/"

-- | Precedence of a binary operator.
precBinOp :: BinaryOp -> Int
precBinOp = \case
    Implies -> 4
    Iff     -> 5
    Or      -> 6
    And     -> 7
    Eq      -> 9
    Neq     -> 9
    Gt      -> 10
    Ge      -> 10
    Lt      -> 10
    Le      -> 10
    Plus    -> 11
    Minus   -> 11
    Times   -> 12
    Divide  -> 12


-- | Unary operators.
data UnaryOp
    = Not
    | Neg -- ^ negation operator
    deriving (Show)

instance Pretty UnaryOp where
    pretty = \case
        Not -> "!"
        Neg -> "-"

-- | Precedence of an unary operator.
precUnOp :: UnaryOp -> Int
precUnOp = \case
    Not -> 8
    Neg -> 13


-- | Temporal operators.
data TemporalOp
    = Next
    | Until
    | Finally
    | Globally
    | WeakUntil
    | Release
    | Cumulative
    | Instant
    | SteadyState
    deriving (Show)

instance Pretty TemporalOp where
    pretty = \case
        Next        -> "X"
        Until       -> "U"
        Finally     -> "F"
        Globally    -> "G"
        WeakUntil   -> "W"
        Release     -> "R"
        Cumulative  -> "C"
        Instant     -> "I"
        SteadyState -> "S"


-- | Relation operators for use with P, R and S operators.
data RelOp
    = RelGt
    | RelGeq
    | RelMin
    | RelLt
    | RelLeq
    | RelMax
    | RelEq
    deriving (Show)

instance Pretty RelOp where
    pretty = \case
        RelGt  -> ">"
        RelGeq -> ">="
        RelMin -> "min="
        RelLt  -> "<"
        RelLeq -> "<="
        RelMax -> "max="
        RelEq  -> "="


-- | Filter operators.
data FilterOp
    = FilterMin
    | FilterMax
    | FilterArgmin
    | FilterArgmax
    | FilterCount
    | FilterSum
    | FilterAvg
    | FilterFirst
    | FilterRange
    | FilterForall
    | FilterExists
    | FilterPrint
    | FilterPrintall
    | FilterState
    deriving (Show)

instance Pretty FilterOp where
    pretty = \case
        FilterMin      -> "min"
        FilterMax      -> "max"
        FilterArgmin   -> "argmin"
        FilterArgmax   -> "argmax"
        FilterCount    -> "count"
        FilterSum      -> "sum"
        FilterAvg      -> "avg"
        FilterFirst    -> "first"
        FilterRange    -> "range"
        FilterForall   -> "forall"
        FilterExists   -> "exists"
        FilterPrint    -> "print"
        FilterPrintall -> "printall"
        FilterState    -> "state"
