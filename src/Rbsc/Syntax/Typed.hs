-- | Typed abstract syntax.
module Rbsc.Syntax.Typed
    ( module Syntax

      -- * Model
    , Model(..)

      -- * Constants
    , TConstant

      -- * Functions
    , TFunction
    , TParameter

      -- * Variables
    , TVarDecl

      -- * Implementation
    , TImplementation
    , TImplBody
    , TModule
    , TModuleBody
    , TCommand
    , TUpdate
    , TAssignment
    , TBody
    , TBodyItem
    , TLoop

      -- * Types
    , TType
    , TVarType

      -- * Initial values
    , TInit

      -- * Expressions
    , LSomeExpr
    ) where


import Data.Map.Strict (Map)


import Rbsc.Data.Name as Syntax

import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Expr.Typed     as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Impl           as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.Type           as Syntax
import Rbsc.Syntax.VarDecl        as Syntax
import Rbsc.Syntax.VarType        as Syntax


-- | Typed abstract syntax of a model.
data Model = Model
    { modelConstants :: [TConstant]
    , modelGlobals   :: [TInit]
    , modelSystem    :: [Loc (Expr Bool)]
    , modelImpls     :: Map TypeName [TModuleBody]
    }


type TConstant = Constant LSomeExpr


type TFunction  = Function LSomeExpr
type TParameter = Parameter LSomeExpr


type TVarDecl = VarDecl LSomeExpr


type TImplementation = Implementation [TInit] TQuantifiedType LSomeExpr
type TImplBody       = ImplBody [TInit] TQuantifiedType LSomeExpr
type TModule         = Module [TInit] TQuantifiedType LSomeExpr
type TModuleBody     = ModuleBody [TInit] TQuantifiedType LSomeExpr

type TCommand    = Command TQuantifiedType LSomeExpr
type TUpdate     = Update TQuantifiedType LSomeExpr
type TAssignment = Assignment LSomeExpr

type TBody a     = Body a TQuantifiedType LSomeExpr
type TBodyItem a = BodyItem a TQuantifiedType LSomeExpr
type TLoop a     = Loop a TQuantifiedType LSomeExpr


type TType    = Type LSomeExpr
type TVarType = VarType LSomeExpr


type TInit = (Name, Maybe LSomeExpr)


type LSomeExpr = Loc SomeExpr
