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
    , TNamedModuleBody
    , TCommand
    , TUpdate
    , TAssignment
    , TElem
    , TElemMulti
    , TLoop

      -- * Types
    , TType
    , TVarType

      -- * Initial values
    , TInits
    , TInit

      -- * Expressions
    , LSomeExpr
    ) where


import Data.Map.Strict (Map)


import Rbsc.Data.Name as Syntax

import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Impl           as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.Type           as Syntax
import Rbsc.Syntax.Typed.Expr     as Syntax
import Rbsc.Syntax.VarDecl        as Syntax
import Rbsc.Syntax.VarType        as Syntax


-- | Typed abstract syntax of a model.
data Model = Model
    { modelConstants :: [TConstant]
    , modelGlobals   :: TInits
    , modelSystem    :: [Loc (Expr Bool)]
    , modelImpls     :: Map TypeName [TNamedModuleBody ElemMulti]
    } deriving (Show)


type TConstant = Constant LSomeExpr


type TFunction  = Function LSomeExpr
type TParameter = Parameter LSomeExpr


type TVarDecl = VarDecl LSomeExpr


type TImplementation       = Implementation ElemMulti TInits TQuantifiedType LSomeExpr
type TImplBody             = ImplBody ElemMulti TInits TQuantifiedType LSomeExpr
type TModule               = Module ElemMulti TInits TQuantifiedType LSomeExpr
type TModuleBody elem      = ModuleBody elem TInits TQuantifiedType LSomeExpr
type TNamedModuleBody elem = NamedModuleBody elem TInits TQuantifiedType LSomeExpr

type TInits = [TInit]
type TInit  = (Name, Maybe LSomeExpr)

type TCommand elem = Command elem TQuantifiedType LSomeExpr
type TUpdate elem  = Update elem TQuantifiedType LSomeExpr
type TAssignment   = Assignment LSomeExpr

type TElem      = Elem TQuantifiedType LSomeExpr
type TElemMulti = ElemMulti TQuantifiedType LSomeExpr
type TLoop      = Loop TQuantifiedType LSomeExpr


type TType    = Type LSomeExpr
type TVarType = VarType LSomeExpr


type LSomeExpr = Loc SomeExpr
