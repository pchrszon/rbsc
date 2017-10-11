{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}


-- | The type system of the modeling language and its embedding into the
-- Haskell type system.
module Rbsc.Type
    (
    -- * Instance names and type names
      Name
    , RoleName
    , TypeName(..)

    -- * Components
    , Component(..)
    , compName
    , compTypeName
    , compBoundTo
    , compContainedIn

    -- * Type system
    , Type(..)
    , AType(..)
    , typeEq
    , (:~:)(..)
    ) where


import Control.Lens

import Data.Map.Strict           (Map)
import Data.String
import Data.Text
import Data.Text.Prettyprint.Doc (Pretty (..))
import Data.Type.Equality        ((:~:) (..))


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

instance IsString TypeName where
    fromString = TypeName . fromString


-- | A component instance (either a natural, a role or a compartment).
data Component = Component
    { _compName        :: !Name
    , _compTypeName    :: !TypeName
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


-- | Check the equality of 'Type's.
typeEq :: Type s -> Type t -> Maybe (s :~: t)
typeEq TyBool      TyBool      = Just Refl
typeEq TyInt       TyInt       = Just Refl
typeEq TyDouble    TyDouble    = Just Refl
typeEq (TyArray s) (TyArray t) = do
    Refl <- typeEq s t
    pure Refl
-- we assume that component types of the same name also have the same local
-- variables
typeEq (TyComponent x _) (TyComponent y _) | x == y = Just Refl
typeEq _ _ = Nothing
