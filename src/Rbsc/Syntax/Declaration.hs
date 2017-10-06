-- | Top-level declarations.
module Rbsc.Syntax.Declaration
    ( Declaration(..)
    ) where


import Rbsc.Syntax.TypeLevel


-- | Top-level declarations of a model.
data Declaration l
    = DeclNaturalType (NaturalTypeDef l)
    | DeclRoleType (RoleTypeDef l)
    | DeclCompartmentType (CompartmentTypeDef l)
    deriving (Show)
