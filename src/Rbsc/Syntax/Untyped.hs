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

      -- * Variables
    , UGlobal
    , UVarDecl

      -- * Implementation
    , UImplementation
    , UImplBody
    , UModule
    , UModuleBody
    , UCommand
    , UUpdate
    , UAssignment
    , UBody
    , UBodyItem
    , ULoop

      -- * Types
    , UType
    , UVarType
    ) where


import Rbsc.Data.ComponentType
import Rbsc.Data.Name          as Syntax

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Expr.Untyped   as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Global         as Syntax
import Rbsc.Syntax.Impl           as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.Type           as Syntax
import Rbsc.Syntax.VarDecl        as Syntax
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
    , modelImplementations  :: [UImplementation]
    , modelModules          :: [UModule]
    } deriving (Show)


type UCompartmentTypeDef = CompartmentTypeDef LExpr
type UMultiRole          = MultiRole LExpr


type UConstant = Constant LExpr


type UFunction  = Function LExpr
type UParameter = Parameter LExpr


type UGlobal  = Global LExpr
type UVarDecl = VarDecl LExpr


type UImplementation = Implementation ComponentTypeSet LExpr
type UImplBody       = ImplBody ComponentTypeSet LExpr
type UModule         = Module ComponentTypeSet LExpr
type UModuleBody     = ModuleBody ComponentTypeSet LExpr

type UCommand    = Command ComponentTypeSet LExpr
type UUpdate     = Update ComponentTypeSet LExpr
type UAssignment = Assignment LExpr

type UBody a     = Body a ComponentTypeSet LExpr
type UBodyItem a = BodyItem a ComponentTypeSet LExpr
type ULoop a     = Loop a ComponentTypeSet LExpr


type UType    = Type LExpr
type UVarType = VarType LExpr
