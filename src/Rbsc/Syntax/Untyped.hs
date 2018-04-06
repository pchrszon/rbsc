-- | Untyped abstract syntax.
module Rbsc.Syntax.Untyped
    ( module Syntax

      -- * Model
    , UModel

      -- * Component Types
    , UCompartmentTypeDef
    , UMultiRole

      -- * Constants
    , UConstant

      -- * Functions
    , UFunction
    , UParameter

      -- * Types
    , UType
    ) where


import Rbsc.Data.Name as Syntax

import Rbsc.Syntax.ComponentType  as Syntax
import Rbsc.Syntax.Constant       as Syntax
import Rbsc.Syntax.Expr.Untyped   as Syntax
import Rbsc.Syntax.Function       as Syntax
import Rbsc.Syntax.Model          as Syntax
import Rbsc.Syntax.Operators      as Syntax
import Rbsc.Syntax.Quantification as Syntax
import Rbsc.Syntax.Type           as Syntax


type UModel = Model LExpr


type UCompartmentTypeDef = CompartmentTypeDef LExpr
type UMultiRole          = MultiRole LExpr


type UConstant = Constant LExpr


type UFunction  = Function LExpr
type UParameter = Parameter LExpr


type UType = Type LExpr
