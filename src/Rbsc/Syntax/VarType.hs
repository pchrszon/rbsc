-- | Abstract syntax of variable types.
module Rbsc.Syntax.VarType
    ( VarType(..)
    ) where


import Rbsc.Data.Name

import Rbsc.Report.Region


-- | Abstract syntax of a variable type.
data VarType expr
    = VarTyBool
    | VarTyInt (expr, expr)
    | VarTyEnum [Loc Name]
    | VarTyArray expr (VarType expr)
    deriving (Show)
