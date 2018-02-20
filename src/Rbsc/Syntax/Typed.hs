-- | Typed abstract syntax.
module Rbsc.Syntax.Typed
    ( module Syntax

      -- * Model
    , TModel

      -- * Constants
    , TConstant

      -- * Functions
    , TFunction
    , TParameter

      -- * Types
    , TType

      -- * Expressions
    , LSomeExpr
    ) where


import Rbsc.Data.Name as Syntax

import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.ComponentType as Syntax
import Rbsc.Syntax.Constant      as Syntax
import Rbsc.Syntax.Expr.Typed    as Syntax
import Rbsc.Syntax.Function      as Syntax
import Rbsc.Syntax.Model         as Syntax
import Rbsc.Syntax.Operators     as Syntax
import Rbsc.Syntax.Type          as Syntax


type TModel = Model LSomeExpr


type TConstant = Constant LSomeExpr


type TFunction  = Function LSomeExpr
type TParameter = Parameter LSomeExpr


type TType = Type LSomeExpr


type LSomeExpr = Loc SomeExpr
