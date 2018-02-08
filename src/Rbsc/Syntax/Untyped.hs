-- | Untyped abstract syntax.
module Rbsc.Syntax.Untyped
    ( module Syntax

      -- * Model
    , UModel

      -- * Constants
    , UConstantDef
    ) where


import Rbsc.Data.Name as Syntax

import Rbsc.Syntax.ComponentType as Syntax
import Rbsc.Syntax.Constant      as Syntax
import Rbsc.Syntax.Expr.Untyped  as Syntax
import Rbsc.Syntax.Model         as Syntax
import Rbsc.Syntax.Operators     as Syntax
import Rbsc.Syntax.Type          as Syntax


type UModel = Model LExpr


type UConstantDef = ConstantDef LExpr
