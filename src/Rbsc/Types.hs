{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


-- | The type system of the modeling language and its embedding into the
-- Haskell type system.
module Rbsc.Types
    (
    -- * Instance names and type names
      Name
    , RoleName
    , TypeName(..)

    -- * User-defined component types
    , ComponentTypes
    , ComponentType(..)

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

import Data.Map.Strict           (Map)
import Data.Set                  (Set)
import Data.Text
import Data.Text.Prettyprint.Doc (Pretty (..))


-- | An instance name.
type Name = Text


-- | A 'Name' that is intended to be a role instance name.
type RoleName = Name


-- | The name of a user-defined component type, role type or compartment type.
newtype TypeName = TypeName
    { getTypeName :: Text
    } deriving (Eq, Ord, Show)

instance Pretty TypeName where
    pretty = pretty . getTypeName


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
    deriving (Show)


-- | A component instance (either a natural, a role or a compartment).
data Component = Component
    { _compTypeName    :: !TypeName
    , _compBoundTo     :: Maybe Name
    , _compContainedIn :: Maybe Name
    } deriving (Show)


makeLenses ''Component


-- | Value-level representation of types.
data Type t where
    TyBool      :: Type Bool
    TyInt       :: Type Integer
    TyDouble    :: Type Double
    TyArray     :: Type t -> Type [t]
    TyComponent :: TypeName -> Map Name AType -> Type Component

deriving instance Show (Type t)


-- | Existentially quantified 'Type'.
data AType where
    AType :: Type t -> AType

deriving instance Show AType
