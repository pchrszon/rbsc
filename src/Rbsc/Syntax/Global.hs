-- | Abstract syntax of global definitions.
module Rbsc.Syntax.Global
    ( Global(..)
    ) where


import Rbsc.Syntax.VarDecl


-- | A definition of a global variable.
newtype Global expr = Global
    { getGlobal :: VarDecl expr
    } deriving (Eq, Ord, Show)
