-- | Abstract syntax of function definitions.
module Rbsc.Syntax.Function
    ( Function(..)
    , Parameter(..)
    ) where


import Data.List.NonEmpty (NonEmpty)


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Type


-- | A function definition.
data Function expr = Function
    { functionName :: Loc Name
    , functionArgs :: NonEmpty Parameter
    , functionType :: Type
    , functionBody :: expr
    } deriving (Show)


-- | A function parameter.
data Parameter = Parameter (Loc Name) Type deriving (Show)
