-- | Abstract syntax of types.
module Rbsc.Syntax.Type
    ( Type(..)
    ) where


-- | Abstract syntax of a type.
data Type
    = TyBool
    | TyInt
    | TyDouble
    | TyArray !Type
    deriving (Show)
