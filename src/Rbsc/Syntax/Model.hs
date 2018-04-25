-- | Top-level syntax of models.
module Rbsc.Syntax.Model
    ( Model(..)
    ) where


import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Constant
import Rbsc.Syntax.Function
import Rbsc.Syntax.Global


-- | Abstract syntax of a model.
data Model expr = Model
    { modelConstants        :: [Constant expr]
    , modelFunctions        :: [Function expr]
    , modelGlobals          :: [Global expr]
    , modelNaturalTypes     :: [NaturalTypeDef]
    , modelRoleTypes        :: [RoleTypeDef]
    , modelCompartmentTypes :: [CompartmentTypeDef expr]
    , modelSystem           :: [expr]
    } deriving (Show)
