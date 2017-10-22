-- | Abstract syntax of untyped expressions.
module Rbsc.Syntax.Expr.Untyped
    ( Expr(..)
    ) where


import Rbsc.Report.Region
import Rbsc.Syntax.Operators
import Rbsc.Type


-- | An untyped expression.
data Expr
    = LitBool !Bool
    | Variable !Name
    | Not (Loc Expr)
    | BoolBinOp !BoolBinOp (Loc Expr) (Loc Expr)
    | HasType (Loc Expr) (Loc TypeName)
    | BoundTo (Loc Expr) (Loc Expr)
    | Element (Loc Expr) (Loc Expr)
    | Quantified !Quantifier !Name (Maybe (Loc TypeName)) (Loc Expr)
    deriving (Show)
