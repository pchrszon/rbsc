-- | Abstract syntax of global definitions.
module Rbsc.Syntax.Global
    ( Global(..)
    ) where


import Data.Function
import Data.Ord


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.VarType


-- | A global definition.
data Global expr = Global
    { globalName :: Loc Name
    , globalType :: VarType expr
    , globalInit :: Maybe expr
    } deriving (Show)

instance Eq (Global expr) where
    (==) = (==) `on` globalName

instance Ord (Global expr) where
    compare = comparing globalName
