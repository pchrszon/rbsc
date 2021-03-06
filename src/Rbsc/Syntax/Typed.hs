-- | Typed abstract syntax.
module Rbsc.Syntax.Typed
    ( module Syntax

      -- * Model
    , Model(..)

      -- * Variables
    , TVarDecl

      -- * Labels
    , TLabel

      -- * Implementation
    , TImplementation
    , TImplBody
    , TModule
    , TModuleBody
    , TModuleInstance
    , TCommand
    , TUpdate
    , TAssignment
    , TElem
    , TElemMulti
    , TLoop

      -- * Coordination
    , TCoordinator
    , TCoordCommand
    , TPlayingConstraint

      -- * Reward structures
    , TRewardStruct
    , TRewardStructItem
    , TRewardKind

      -- * Initial values
    , TInits
    , TInit

      -- * Expressions
    , LSomeExpr
    ) where


import Data.Map.Strict (Map)


import Rbsc.Data.Component
import Rbsc.Data.Name      as Syntax

import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Coordinator    as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Impl           as Syntax
import Rbsc.Syntax.Label          as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.RewardStruct   as Syntax
import Rbsc.Syntax.Type           as Syntax
import Rbsc.Syntax.Typed.Expr     as Syntax
import Rbsc.Syntax.VarDecl        as Syntax
import Rbsc.Syntax.VarType        as Syntax


-- | Typed abstract syntax of a model.
data Model = Model
    { modelGlobals       :: TInits
    , modelLabels        :: [TLabel]
    , modelSystem        :: [Loc (Expr Bool)]
    , modelImpls         :: Map TypeName [TModuleInstance ElemMulti]
    , modelCoordinators  :: [TCoordinator ElemMulti]
    , modelRewardStructs :: [TRewardStruct ElemMulti]
    , modelObserve       :: [Loc (Expr Component)]
    } deriving (Show)


type TVarDecl = VarDecl LSomeExpr


type TLabel = Label LSomeExpr


type TImplementation       = Implementation ElemMulti TInits TQuantifiedType LSomeExpr
type TImplBody             = ImplBody ElemMulti TInits TQuantifiedType LSomeExpr
type TModule               = Module ElemMulti TInits TQuantifiedType LSomeExpr
type TModuleBody elem      = ModuleBody elem TInits TQuantifiedType LSomeExpr
type TModuleInstance elem  = ModuleInstance elem TInits TQuantifiedType LSomeExpr (Name, LSomeExpr)

type TCoordinator elem  = Coordinator elem TInits TQuantifiedType LSomeExpr
type TCoordCommand elem = CoordCommand elem TQuantifiedType LSomeExpr
type TPlayingConstraint = PlayingConstraint LSomeExpr

type TInits = [TInit]
type TInit  = (Name, Maybe LSomeExpr)

type TCommand elem = Command elem TQuantifiedType LSomeExpr
type TUpdate elem  = Update elem TQuantifiedType LSomeExpr
type TAssignment   = Assignment LSomeExpr

type TElem      = Elem TQuantifiedType LSomeExpr
type TElemMulti = ElemMulti TQuantifiedType LSomeExpr
type TLoop      = Loop TQuantifiedType LSomeExpr


type TRewardStruct elem = RewardStruct elem TQuantifiedType LSomeExpr
type TRewardStructItem  = RewardStructItem LSomeExpr
type TRewardKind        = RewardKind LSomeExpr


type LSomeExpr = Loc SomeExpr
