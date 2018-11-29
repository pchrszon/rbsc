-- | Abstract syntax of function definitions.
module Rbsc.Syntax.Function
    ( Function(..)
    , Parameter(..)
    ) where


import Data.Ord


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Type


-- | A function definition.
data Function expr = Function
    { functionForType :: Maybe (Loc TypeName)
    , functionName    :: Loc Name
    , functionParams  :: [Parameter expr]
    , functionType    :: Type expr
    , functionBody    :: expr
    } deriving (Show)

instance Eq (Function expr) where
    l == r =
        functionName l == functionName r &&
        functionForType l == functionForType r

instance Ord (Function expr) where
    compare = comparing functionName <> comparing functionForType


-- | A function parameter.
data Parameter expr = Parameter
    { paramName :: Loc Name
    , paramType :: Type expr
    } deriving (Show)
