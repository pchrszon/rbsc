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

      -- * Labels
    , ULabel

      -- * Implementation
    , UImplementation
    , UImplBody
    , UModuleRef
    , UModule
    , UModuleBody
    , UModuleInstance
    , UCommand
    , UUpdate
    , UAssignment
    , UElem
    , UElemMulti
    , ULoop
    , UQuantifiedType

      -- * Coordination
    , UCoordinator
    , UCoordCommand
    , UPlayingConstraint

      -- * Reward structures
    , URewardStruct
    , URewardStructItem
    , URewardKind

      -- * Observable roles
    , UObserve

      -- * Types
    , UType
    , UVarType
    ) where


import Rbsc.Data.ComponentType
import Rbsc.Data.Name          as Syntax

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Coordinator    as Syntax
import Rbsc.Syntax.Enumeration    as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Impl           as Syntax
import Rbsc.Syntax.Label          as Syntax
import Rbsc.Syntax.Observe        as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.RewardStruct   as Syntax
import Rbsc.Syntax.Type           as Syntax
import Rbsc.Syntax.Untyped.Expr   as Syntax
import Rbsc.Syntax.VarDecl        as Syntax
import Rbsc.Syntax.VarType        as Syntax

import Rbsc.Syntax.Typed (LSomeExpr)


-- | Untyped abstract syntax of a model.
data Model = Model
    { modelConstants        :: [UConstant]
    , modelEnumumerations   :: [Enumeration]
    , modelFunctions        :: [UFunction]
    , modelGlobals          :: [UVarDecl]
    , modelLabels           :: [ULabel]
    , modelNaturalTypes     :: [NaturalTypeDef]
    , modelRoleTypes        :: [RoleTypeDef]
    , modelCompartmentTypes :: [UCompartmentTypeDef]
    , modelTypeSets         :: [TypeSetDef]
    , modelSystem           :: [LExpr]
    , modelModules          :: [UModule]
    , modelImpls            :: [UImplementation]
    , modelCoordinators     :: [UCoordinator]
    , modelRewardStructs    :: [URewardStruct]
    , modelObserve          :: [LExpr]
    } deriving (Show)


type UCompartmentTypeDef = CompartmentTypeDef LExpr
type UMultiRole          = MultiRole LExpr


type UConstant = Constant LExpr


type UFunction  = Function LExpr
type UParameter = Parameter LExpr


type UVarDecl = VarDecl LExpr


type ULabel = Label LExpr


type UImplementation  = Implementation ElemMulti UVarDecls UQuantifiedType LExpr
type UImplBody        = ImplBody ElemMulti UVarDecls UQuantifiedType LExpr
type UModuleRef       = ModuleRef LExpr
type UModule          = Module ElemMulti UVarDecls UQuantifiedType LExpr
type UModuleBody      = ModuleBody ElemMulti UVarDecls UQuantifiedType LExpr
type UModuleInstance  = ModuleInstance ElemMulti UVarDecls UQuantifiedType LExpr (Name, LSomeExpr)

type UCoordinator       = Coordinator ElemMulti UVarDecls UQuantifiedType LExpr
type UCoordCommand      = CoordCommand ElemMulti UQuantifiedType LExpr
type UPlayingConstraint = PlayingConstraint LExpr

type UVarDecls = [VarDecl LExpr]

type UCommand    = Command ElemMulti UQuantifiedType LExpr
type UUpdate     = Update ElemMulti UQuantifiedType LExpr
type UAssignment = Assignment LExpr

type UElem      = Elem UQuantifiedType LExpr
type UElemMulti = ElemMulti UQuantifiedType LExpr
type ULoop      = Loop UQuantifiedType LExpr

type UQuantifiedType = QuantifiedType ComponentTypeSet LExpr


type URewardStruct     = RewardStruct ElemMulti UQuantifiedType LExpr
type URewardStructItem = RewardStructItem LExpr
type URewardKind       = RewardKind LExpr


type UObserve = Observe LExpr


type UType    = Type LExpr
type UVarType = VarType LExpr
