-- | Abstract syntax of constant definitions.
module Rbsc.Syntax.Constant
    ( ConstantDef(..)
    ) where


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Expr.Untyped
import Rbsc.Syntax.Type


-- | A definition of a constant.
data ConstantDef = ConstantDef
    { constName :: Loc Name
    , constType :: !Type
    , constExpr :: Loc Expr
    } deriving (Show)
