{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}


module Rbsc.Value
    (
    -- * Values
      Value(..)

    -- * Constants
    , Constants
    ) where


import Data.Map.Strict (Map)


import Rbsc.Name
import Rbsc.Type


-- | A value tagged with its 'Type'.
data Value where
    Value :: Show t => t -> Type t -> Value

deriving instance Show Value


-- | A constant has a 'Name' and a 'Value'.
type Constants = Map Name Value
