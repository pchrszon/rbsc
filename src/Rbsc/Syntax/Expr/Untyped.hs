-- | Abstract syntax of untyped expressions.
module Rbsc.Syntax.Expr.Untyped
    ( Expr(..)
    ) where


import Data.List.NonEmpty (NonEmpty)


import Rbsc.Data.Function
import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Operators


-- | An untyped expression.
data Expr
    = LitBool !Bool
    | LitInt !Integer
    | LitDouble !Double
    | Function !FunctionSym
    | Array (NonEmpty (Loc Expr))
    | Variable !Name
    | Not (Loc Expr)
    | Negate (Loc Expr)
    | ArithOp !ArithOp (Loc Expr) (Loc Expr)
    | Divide (Loc Expr) (Loc Expr)
    | EqOp !EqOp (Loc Expr) (Loc Expr)
    | RelOp !RelOp (Loc Expr) (Loc Expr)
    | LogicOp !LogicOp (Loc Expr) (Loc Expr)
    | Index (Loc Expr) (Loc Expr)
    | Call (Loc Expr) [Loc Expr]
    | HasType (Loc Expr) (Loc TypeName)
    | BoundTo (Loc Expr) (Loc Expr)
    | Element (Loc Expr) (Loc Expr)
    | Quantified !Quantifier !Name (Maybe (Loc TypeName)) (Loc Expr)
    deriving (Show)
