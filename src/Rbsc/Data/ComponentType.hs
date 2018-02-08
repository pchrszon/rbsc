{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}


-- | User-defined natural types, role types and compartment types.
module Rbsc.Data.ComponentType
    ( ComponentTypes
    , ComponentType(..)

    , fromModel
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Foldable   (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import Rbsc.Data.Name

import qualified Rbsc.Report.Error.Syntax as Syntax
import           Rbsc.Report.Region

import           Rbsc.Syntax.ComponentType
import           Rbsc.Syntax.Model         (Model)
import qualified Rbsc.Syntax.Model         as Model


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


-- | Extract 'ComponentTypes' from a 'Model'.
fromModel :: Model expr -> Either [Syntax.Error] ComponentTypes
fromModel model =
    let (types, errors) = convert model
        moreErrors = validate types model
        allErrors = errors ++ moreErrors
    in if null allErrors
           then Right types
           else Left allErrors


convert :: Model expr -> (ComponentTypes, [Syntax.Error])
convert model =
    over _1 (fmap fst) . flip execState (Map.empty, []) $ do
        for_ (Model.naturalTypes model) $ \(NaturalTypeDef (Loc name rgn)) ->
            insertType name NaturalType rgn

        for_ (Model.roleTypes model) $
            \(RoleTypeDef (Loc name rgn) playerTyNames) ->
                insertType
                    name
                    (RoleType (Set.fromList (fmap unLoc playerTyNames)))
                    rgn

        for_ (Model.compartmentTypes model) $
            \(CompartmentTypeDef (Loc name rgn) roleTyNames) ->
                insertType name (CompartmentType (fmap unLoc roleTyNames)) rgn
  where
    insertType ::
           TypeName
        -> ComponentType
        -> Region
        -> State (Map TypeName (ComponentType, Region), [Syntax.Error]) ()
    insertType name ty rgn =
        use (_1.at name) >>= \case
            Just (_, rgnFirst) -> throw (Syntax.DuplicateType rgn rgnFirst)
            Nothing -> _1.at name .= Just (ty, rgn)

    throw e = modifying _2 (++ [e])


validate :: ComponentTypes -> Model expr -> [Syntax.Error]
validate types model =
    validateRoleTypes (Model.roleTypes model) ++
    validateCompartmentTypes (Model.compartmentTypes model)
  where
    validateRoleTypes = concatMap $ \(RoleTypeDef _ playerTyNames) ->
        mapMaybe exists playerTyNames

    validateCompartmentTypes = concatMap $
        \(CompartmentTypeDef _ roleTyNames) ->
            mapMaybe isRoleType roleTyNames

    exists (Loc tyName rgn)
        | Map.member tyName types = Nothing
        | otherwise = Just (Syntax.UndefinedType rgn)

    isRoleType (Loc tyName rgn) = case Map.lookup tyName types of
        Just (RoleType _) -> Nothing
        Just _            -> Just (Syntax.NonRoleInCompartment rgn)
        Nothing           -> Just (Syntax.UndefinedType rgn)
