-- | Top-level syntax of models.
module Rbsc.Syntax.Model
    ( Model(..)
    ) where


import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Constant
import Rbsc.Syntax.Expr.Untyped


-- | Abstract syntax of a model.
data Model = Model
    { constants        :: [ConstantDef]
    , naturalTypes     :: [NaturalTypeDef]
    , roleTypes        :: [RoleTypeDef]
    , compartmentTypes :: [CompartmentTypeDef]
    , system           :: [Loc Expr]
    } deriving (Show)
