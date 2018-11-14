{-# LANGUAGE FlexibleContexts #-}


-- | This module validates the component types defined in the model.
module Rbsc.TypeChecker.ComponentTypes
    ( validateComponentTypes
    ) where


import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)


import Rbsc.Data.ComponentType

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Untyped


validateComponentTypes :: ComponentTypes -> Model -> Result ()
validateComponentTypes types model
    | null errors = return ()
    | otherwise   = throwMany errors
  where
    errors = validateCompartmentTypes (modelCompartmentTypes model)

    validateCompartmentTypes = concatMap $
        \(CompartmentTypeDef _ multiRoleLists) ->
            concatMap (mapMaybe checkIfRoleType) multiRoleLists

    checkIfRoleType (MultiRole (Loc tyName rgn) _) =
        case Map.lookup tyName types of
            Just (RoleType _) -> Nothing
            Just _            -> Just (locError rgn NonRoleInCompartment)
            Nothing           -> Just (locError rgn UndefinedType)
