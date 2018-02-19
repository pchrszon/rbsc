{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}


-- | User-defined natural types, role types and compartment types.
module Rbsc.Data.ComponentType
    ( ComponentTypes
    , ComponentType(..)
    ) where


import Data.Map.Strict (Map)
import Data.Set        (Set)


import Rbsc.Data.Name


-- | User-defined component types indexed by their name.
type ComponentTypes = Map TypeName ComponentType


-- | Represents a user-defined component type.
data ComponentType
      -- | A natural type.
    = NaturalType
      -- | A role type with its set of possible player types.
    | RoleType (Set TypeName)
      -- | A compartment type with its list of required role types.
    | CompartmentType [TypeName]
    deriving (Eq, Show)
