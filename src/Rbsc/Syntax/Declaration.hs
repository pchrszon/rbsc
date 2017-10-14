-- | Top-level declarations.
module Rbsc.Syntax.Declaration
    ( Declaration(..)
    ) where


import Rbsc.Syntax.TypeLevel


-- | Top-level declarations of a model.
data Declaration
    = DeclNaturalType NaturalTypeDef
    | DeclRoleType RoleTypeDef
    | DeclCompartmentType CompartmentTypeDef
    deriving (Show)
