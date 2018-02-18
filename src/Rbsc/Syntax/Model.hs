-- | Top-level syntax of models.
module Rbsc.Syntax.Model
    ( Model(..)
    ) where


import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Constant
import Rbsc.Syntax.Function


-- | Abstract syntax of a model.
data Model expr = Model
    { modelConstants        :: [Constant expr]
    , modelFunctions        :: [Function expr]
    , modelNaturalTypes     :: [NaturalTypeDef]
    , modelRoleTypes        :: [RoleTypeDef]
    , modelCompartmentTypes :: [CompartmentTypeDef]
    , modelSystem           :: [expr]
    } deriving (Show)
