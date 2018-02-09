-- | Top-level syntax of models.
module Rbsc.Syntax.Model
    ( Model(..)
    ) where


import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Constant


-- | Abstract syntax of a model.
data Model expr = Model
    { constants        :: [Constant expr]
    , naturalTypes     :: [NaturalTypeDef]
    , roleTypes        :: [RoleTypeDef]
    , compartmentTypes :: [CompartmentTypeDef]
    , system           :: [expr]
    } deriving (Show)
