-- | Abstract syntax of constant definitions.
module Rbsc.Syntax.Constant
    ( Constant(..)
    ) where


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Type


-- | A constant definition.
data Constant expr = Constant
    { constName :: Loc Name
    , constType :: !Type
    , constExpr :: expr
    } deriving (Show)
