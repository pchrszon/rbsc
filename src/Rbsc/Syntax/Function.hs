-- | Abstract syntax of function definitions.
module Rbsc.Syntax.Function
    ( Function(..)
    , Parameter(..)
    ) where


import Data.Function
import Data.Ord


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Type


-- | A function definition.
data Function expr = Function
    { functionName   :: Loc Name
    , functionParams :: [Parameter expr]
    , functionType   :: Type expr
    , functionBody   :: expr
    } deriving (Show)

instance Eq (Function expr) where
    (==) = (==) `on` functionName

instance Ord (Function expr) where
    compare = comparing functionName


-- | A function parameter.
data Parameter expr = Parameter
    { paramName :: Loc Name
    , paramType :: Type expr
    } deriving (Show)
