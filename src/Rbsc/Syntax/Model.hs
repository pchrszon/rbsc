-- | Top-level syntax of models.
module Rbsc.Syntax.Model
    ( Model(..)
    ) where


import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Constant
import Rbsc.Syntax.Function


-- | Abstract syntax of a model.
data Model expr = Model
    { constants        :: [Constant expr]
    , functions        :: [Function expr]
    , naturalTypes     :: [NaturalTypeDef]
    , roleTypes        :: [RoleTypeDef]
    , compartmentTypes :: [CompartmentTypeDef]
    , system           :: [expr]
    } deriving (Show)
