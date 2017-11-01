{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Type-tagged values.
module Rbsc.Data.Value
    (
    -- * Values
      Value(..)

    -- * Constants
    , Constants
    ) where


import Data.Map.Strict (Map)


import Rbsc.Data.Name
import Rbsc.Data.Type


-- | A value tagged with its 'Type'.
data Value where
    Value :: Show t => t -> Type t -> Value

deriving instance Show Value


-- | A constant has a 'Name' and a 'Value'.
type Constants = Map Name Value
