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
    , UVarDecl

      -- * Implementation
    , UImplementation
    , UImplBody
    , UModule
    , UModuleBody
    , UCommand
    , UUpdate
    , UAssignment
    , UElem
    , UElemMulti
    , ULoop
    , UQuantifiedType

      -- * Types
    , UType
    , UVarType
    ) where


import Data.Map.Strict (Map)


import Rbsc.Data.ComponentType
import Rbsc.Data.Name          as Syntax

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Impl           as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.Type           as Syntax
import Rbsc.Syntax.Untyped.Expr   as Syntax
import Rbsc.Syntax.VarDecl        as Syntax
import Rbsc.Syntax.VarType        as Syntax


-- | Untyped abstract syntax of a model.
data Model = Model
    { modelConstants        :: [UConstant]
    , modelFunctions        :: [UFunction]
    , modelGlobals          :: [UVarDecl]
    , modelNaturalTypes     :: [NaturalTypeDef]
    , modelRoleTypes        :: [RoleTypeDef]
    , modelCompartmentTypes :: [UCompartmentTypeDef]
    , modelSystem           :: [LExpr]
    , modelImpls            :: Map TypeName [UModuleBody]
    } deriving (Show)


type UCompartmentTypeDef = CompartmentTypeDef LExpr
type UMultiRole          = MultiRole LExpr


type UConstant = Constant LExpr


type UFunction  = Function LExpr
type UParameter = Parameter LExpr


type UVarDecl = VarDecl LExpr


type UImplementation = Implementation ElemMulti UVarDecls UQuantifiedType LExpr
type UImplBody       = ImplBody ElemMulti UVarDecls UQuantifiedType LExpr
type UModule         = Module ElemMulti UVarDecls UQuantifiedType LExpr
type UModuleBody     = ModuleBody ElemMulti UVarDecls UQuantifiedType LExpr

type UVarDecls = [VarDecl LExpr]

type UCommand    = Command ElemMulti UQuantifiedType LExpr
type UUpdate     = Update ElemMulti UQuantifiedType LExpr
type UAssignment = Assignment LExpr

type UElem      = Elem UQuantifiedType LExpr
type UElemMulti = ElemMulti UQuantifiedType LExpr
type ULoop      = Loop UQuantifiedType LExpr

type UQuantifiedType = QuantifiedType ComponentTypeSet LExpr


type UType    = Type LExpr
type UVarType = VarType LExpr
