-- | Abstract syntax of constant definitions.
module Rbsc.Syntax.Constant
    ( Constant(..)
    ) where


import Data.Function
import Data.Ord


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Type


-- | A constant definition.
data Constant expr = Constant
    { constName :: Loc Name
    , constType :: Maybe (Type expr)
    , constExpr :: expr
    } deriving (Show)

instance Eq (Constant expr) where
    (==) = (==) `on` constName

instance Ord (Constant expr) where
    compare = comparing constName
