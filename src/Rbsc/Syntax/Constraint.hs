module Rbsc.Syntax.Constraint where


import Rbsc.Report.Region
import Rbsc.Syntax.Operators
import Rbsc.Type


-- | A constraint expression.
data Constraint
    = LitBool !Bool
    | Variable !Name
    | Not (Loc Constraint)
    | BoolBinOp !BoolBinOp (Loc Constraint) (Loc Constraint)
    | HasType (Loc Constraint) (Loc TypeName)
    | BoundTo (Loc Constraint) (Loc Constraint)
    | Element (Loc Constraint) (Loc Constraint)
    | Quantified !Quantifier !Name (Maybe TypeName) (Loc Constraint)
    deriving (Show)
