-- | Abstract syntax of untyped expressions.
module Rbsc.Syntax.Expr.Untyped
    ( Expr(..)
    , LExpr
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
    | LitFunction !FunctionName
    | Array (NonEmpty LExpr)
    | Variable !Name
    | Not LExpr
    | Negate LExpr
    | ArithOp !ArithOp LExpr LExpr
    | Divide LExpr LExpr
    | EqOp !EqOp LExpr LExpr
    | RelOp !RelOp LExpr LExpr
    | LogicOp !LogicOp LExpr LExpr
    | Index LExpr LExpr
    | Call LExpr [LExpr]
    | HasType LExpr (Loc TypeName)
    | BoundTo LExpr LExpr
    | Element LExpr LExpr
    | Quantified !Quantifier !Name (Maybe (Loc TypeName)) LExpr
    deriving (Show)


-- | A location-annotated 'Expr'.
type LExpr = Loc Expr
