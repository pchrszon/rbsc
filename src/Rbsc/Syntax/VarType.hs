-- | Abstract syntax of variable types.
module Rbsc.Syntax.VarType
    ( VarType(..)
    ) where


-- | Abstract syntax of a variable type.
data VarType expr
    = VarTyBool
    | VarTyInt (expr, expr)
    | VarTyArray (expr, expr) (VarType expr)
    deriving (Show)
