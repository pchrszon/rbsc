{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


-- | The type system of the modeling language and its embedding into the
-- Haskell type system.
module Types
    (
    -- * Identifiers and names
      Ident
    , TypeName(..)

    -- * User-defined component types
    , ComponentTypes

    , ComponentType(..)
    , ctyName
    , ctyInfo

    , ComponentTypeInfo(..)

    -- * Components
    , Component(..)
    , compTypeName
    , compBoundTo
    , compContainedIn

    -- * Type system
    , Type(..)
    , AType(..)
    ) where


import Control.Lens

import qualified Data.Map.Lazy   as L
import qualified Data.Map.Strict as S
import           Data.Set        (Set)
import           Data.Text


-- | An identifier.
type Ident = Text


-- | The name of a user-defined component type, role type or compartment type.
newtype TypeName = TypeName
    { getTypeName :: Text
    } deriving (Eq, Ord, Show)


-- | User-defined component types indexed by their name.
type ComponentTypes = L.Map TypeName ComponentType


-- | Represents a user-defined component type.
data ComponentType = ComponentType
    { _ctyName :: !TypeName
    , _ctyInfo :: !ComponentTypeInfo
    } deriving (Show)


-- | Additional information about a user-defined component type.
data ComponentTypeInfo
      -- | A natural type.
    = NaturalType
      -- | A role type with its set of possible player types.
    | RoleType (Set ComponentType)
      -- | A compartment type with its list of required role types.
    | CompartmentType [ComponentType]
    deriving (Show)


-- | A component instance (either a natural, a role or a compartment).
data Component = Component
    { _compTypeName    :: !TypeName
    , _compBoundTo     :: Maybe TypeName
    , _compContainedIn :: Maybe TypeName
    } deriving (Show)


-- | Value-level representation of types.
data Type t where
    TyBool      :: Type Bool
    TyInt       :: Type Integer
    TyDouble    :: Type Double
    TyArray     :: Type t -> Type [t]
    TyComponent :: TypeName -> S.Map Ident AType -> Type Component

deriving instance Show (Type t)


-- | Existentially quantified 'Type'.
data AType where
    AType :: Type t -> AType

deriving instance Show AType


makeLenses ''ComponentType
makeLenses ''Component
