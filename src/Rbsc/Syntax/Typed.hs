-- | Typed abstract syntax.
module Rbsc.Syntax.Typed
    ( module Syntax

      -- * Model
    , TModel

      -- * Constants
    , TConstant

      -- * Expressions
    , LSomeExpr
    ) where


import Rbsc.Data.Name as Syntax

import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.ComponentType as Syntax
import Rbsc.Syntax.Constant      as Syntax
import Rbsc.Syntax.Expr.Typed    as Syntax
import Rbsc.Syntax.Model         as Syntax
import Rbsc.Syntax.Operators     as Syntax
import Rbsc.Syntax.Type          as Syntax


type TModel = Model LSomeExpr


type TConstant = Constant LSomeExpr


type LSomeExpr = Loc SomeExpr