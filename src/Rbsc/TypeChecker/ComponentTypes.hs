{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}


-- | This module extracts and validates the component types defined in the
-- model.
module Rbsc.TypeChecker.ComponentTypes
    ( getComponentTypes
    ) where


import Control.Lens
import Control.Monad.State.Strict

import           Data.Foldable   (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import qualified Data.Set        as Set


import Rbsc.Data.ComponentType
import Rbsc.Data.Name

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Model


-- | Extract 'ComponentTypes' from a 'Model'.
getComponentTypes :: Model expr -> Either [Error] ComponentTypes
getComponentTypes model =
    let (types, errors) = convert model
        moreErrors = validate types model
        allErrors = errors ++ moreErrors
    in if null allErrors
           then Right types
           else Left allErrors


convert :: Model expr -> (ComponentTypes, [Error])
convert model =
    over _1 (fmap fst) . flip execState (Map.empty, []) $ do
        for_ (modelNaturalTypes model) $ \(NaturalTypeDef (Loc name rgn)) ->
            insertType name NaturalType rgn

        for_ (modelRoleTypes model) $
            \(RoleTypeDef (Loc name rgn) playerTyNames) ->
                insertType
                    name
                    (RoleType (Set.fromList (fmap unLoc playerTyNames)))
                    rgn

        for_ (modelCompartmentTypes model) $
            \(CompartmentTypeDef (Loc name rgn) roleTyNames) ->
                insertType name (CompartmentType (fmap unLoc roleTyNames)) rgn
  where
    insertType ::
           TypeName
        -> ComponentType
        -> Region
        -> State (Map TypeName (ComponentType, Region), [Error]) ()
    insertType name ty rgn =
        use (_1.at name) >>= \case
            Just (_, rgnFirst) -> throw' (Error rgn (DuplicateType rgnFirst))
            Nothing -> _1.at name .= Just (ty, rgn)

    throw' e = modifying _2 (++ [e])


validate :: ComponentTypes -> Model expr -> [Error]
validate types model =
    validateRoleTypes (modelRoleTypes model) ++
    validateCompartmentTypes (modelCompartmentTypes model)
  where
    validateRoleTypes = concatMap $ \(RoleTypeDef _ playerTyNames) ->
        mapMaybe exists playerTyNames

    validateCompartmentTypes = concatMap $
        \(CompartmentTypeDef _ roleTyNames) ->
            mapMaybe isRoleType roleTyNames

    exists (Loc tyName rgn)
        | Map.member tyName types = Nothing
        | otherwise = Just (Error rgn UndefinedType)

    isRoleType (Loc tyName rgn) = case Map.lookup tyName types of
        Just (RoleType _) -> Nothing
        Just _            -> Just (Error rgn NonRoleInCompartment)
        Nothing           -> Just (Error rgn UndefinedType)