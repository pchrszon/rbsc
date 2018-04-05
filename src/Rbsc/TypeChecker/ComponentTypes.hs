{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}


-- | This module extracts and validates the component types defined in the
-- model.
module Rbsc.TypeChecker.ComponentTypes
    ( validateComponentTypes
    ) where


import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)


import Rbsc.Data.ComponentType

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Model


validateComponentTypes :: ComponentTypes -> Model expr -> Either [Error] ()
validateComponentTypes types model
    | null errors = Right ()
    | otherwise   = Left errors
  where
    errors =
        validateRoleTypes (modelRoleTypes model) ++
        validateCompartmentTypes (modelCompartmentTypes model)

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
