{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}


-- | User-defined natural types, role types and compartment types.
module Rbsc.Data.ComponentType
    ( ComponentType(..)
    , RoleRef(..)

    , _NaturalType
    , _RoleType
    , _CompartmentType

    , ComponentTypes
    , componentTypes

    , TypeSets
    , typeSets

    , isRoleType
    , isCompartmentType

    , ComponentTypeSet(..)
    , normalizeTypeSet
    ) where


import Control.Lens

import           Data.Foldable   (toList, traverse_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set


import Rbsc.Data.Field
import Rbsc.Data.Name

import Rbsc.Report.Error
import Rbsc.Report.Region


-- | Represents a user-defined component type.
data ComponentType
      -- | A natural type.
    = NaturalType
      -- | A role type with its set of possible player types.
    | RoleType (Set TypeName)
      -- | A compartment type with its list of required role types.
    | CompartmentType [[RoleRef]]
    deriving (Eq, Show)


data RoleRef = RoleRef
    { refType   :: TypeName
    , refBounds :: (Int, Int)
    } deriving (Eq, Show)


makePrisms ''ComponentType


-- | User-defined component types indexed by their name.
type ComponentTypes = Map TypeName ComponentType


-- | A 'Lens' for accessing the 'ComponentTypes'.
componentTypes :: Has ComponentTypes r => Lens' r ComponentTypes
componentTypes = field


-- | User-defined sets of component types indexed by their name.
type TypeSets = Map TypeName (Set TypeName)


-- | A 'Lens' for accessing the 'TypeSets'.
typeSets :: Has TypeSets r => Lens' r TypeSets
typeSets = field


isRoleType :: ComponentTypes -> TypeName -> Bool
isRoleType compTys tyName = has (at tyName._Just._RoleType) compTys


isCompartmentType :: ComponentTypes -> TypeName -> Bool
isCompartmentType compTys tyName =
    has (at tyName._Just._CompartmentType) compTys


-- | A set of component types.
data ComponentTypeSet
    = AllComponents
    | AllNaturals
    | AllRoles
    | AllCompartments
    | ComponentTypeSet (Set (Loc TypeName))
    deriving (Show)


-- | Convert a 'ComponentTypeSet' into a list of 'TypeName's. If one of the
-- types does not exist, an 'UndefinedType' error is thrown.
normalizeTypeSet
    :: ComponentTypes
    -> TypeSets
    -> ComponentTypeSet
    -> Either Error (Set TypeName)
normalizeTypeSet compTys tySets =
    \case
        AllComponents   -> return (Map.keysSet compTys)
        AllNaturals     -> return (filterType _NaturalType)
        AllRoles        -> return (filterType _RoleType)
        AllCompartments -> return (filterType _CompartmentType)
        ComponentTypeSet (toList -> [Loc tyName _])
            | Just tyNames <- Map.lookup tyName tySets -> return tyNames
        ComponentTypeSet (toList -> tyNames) -> do
            traverse_ exists tyNames
            return (Set.fromList (fmap unLoc tyNames))
  where
    exists (Loc tyName rgn)
        | Map.member tyName compTys = return ()
        | otherwise = throw rgn UndefinedType

    filterType p = Map.keysSet (Map.filter (has p) compTys)
