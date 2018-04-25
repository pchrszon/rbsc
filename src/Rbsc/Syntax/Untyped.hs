-- | Untyped abstract syntax.
module Rbsc.Syntax.Untyped
    ( module Syntax

      -- * Model
    , Model(..)

      -- * Component Types
    , UCompartmentTypeDef
    , UMultiRole

      -- * Constants
    , UConstant

      -- * Functions
    , UFunction
    , UParameter

      -- * Globals
    , UGlobal

      -- * Types
    , UType
    , UVarType
    ) where


import Rbsc.Data.Name as Syntax

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Expr.Untyped   as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Global         as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.Type           as Syntax
import Rbsc.Syntax.VarType        as Syntax


-- | Untyped abstract syntax of a model.
data Model = Model
    { modelConstants        :: [UConstant]
    , modelFunctions        :: [UFunction]
    , modelGlobals          :: [UGlobal]
    , modelNaturalTypes     :: [NaturalTypeDef]
    , modelRoleTypes        :: [RoleTypeDef]
    , modelCompartmentTypes :: [UCompartmentTypeDef]
    , modelSystem           :: [LExpr]
    } deriving (Show)


type UCompartmentTypeDef = CompartmentTypeDef LExpr
type UMultiRole          = MultiRole LExpr


type UConstant = Constant LExpr


type UFunction  = Function LExpr
type UParameter = Parameter LExpr


type UGlobal = Global LExpr


type UType    = Type LExpr
type UVarType = VarType LExpr
