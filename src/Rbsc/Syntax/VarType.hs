-- | Abstract syntax of variable types.
module Rbsc.Syntax.VarType
    ( VarType(..)
    ) where


import Rbsc.Syntax.Enumeration


-- | Abstract syntax of a variable type.
data VarType expr
    = VarTyBool
    | VarTyInt (expr, expr)
    | VarTyEnum Enumeration
    | VarTyArray expr (VarType expr)
    deriving (Show)
