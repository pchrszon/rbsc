{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Representation of system instances.
module Rbsc.Data.System
    ( System(..)
    , instances
    , boundTo
    , containedIn

    , instancesOfType
    , boundRoles
    , containedRoles

    , toConstants
    ) where


import Control.Lens
import Data.Map.Strict           (Map, assocs, mapWithKey)
import Data.Text.Prettyprint.Doc

import Rbsc.Data.Component
import Rbsc.Data.Name
import Rbsc.Data.Type
import Rbsc.Data.Value

import Rbsc.Util


-- | A system instance that assigns a type to each component, role and
-- compartment. Also specifies role bindings and compartment membership.
--
-- A system instance may be incomplete, i.e., roles may be unbound and
-- compartments may be missing certain role instances.
data System = System
    { _instances   :: Map Name TypeName
    , _boundTo     :: Map RoleName Name
    , _containedIn :: Map RoleName Name
    } deriving (Show)

makeLenses ''System

instance Pretty System where
    pretty sys =
        sep (punctuate comma (instanceDocs ++ bindingDocs ++ containmentDocs))
      where
        instanceDocs = fmap prettyInstance (view (instances.to assocs) sys)
        bindingDocs = fmap prettyBinding (view (boundTo.to assocs) sys)
        containmentDocs =
            fmap prettyContained (view (containedIn.to assocs) sys)

prettyInstance :: (Name, TypeName) -> Doc ann
prettyInstance (name, tyName) = pretty name <+> colon <+> pretty tyName

prettyBinding :: (RoleName, Name) -> Doc ann
prettyBinding (roleName, playerName) =
    pretty roleName <+> "boundto" <+> pretty playerName

prettyContained :: (RoleName, Name) -> Doc ann
prettyContained (roleName, compartmentName) =
    pretty roleName <+> "in" <+> pretty compartmentName


-- | Get all instances that have the given type.
instancesOfType :: TypeName -> System -> [Name]
instancesOfType tyName = inverseLookup tyName . view instances


-- | Get all roles that are bound to a given player.
boundRoles :: Name -> System -> [RoleName]
boundRoles name = inverseLookup name . view boundTo


-- | Get all roles contained in a given compartment.
containedRoles :: Name -> System -> [RoleName]
containedRoles name = inverseLookup name . view containedIn


-- | Transform a 'System' instance into a 'Constant' table containing
-- a constant 'Value' for each 'Component' instance.
toConstants :: System -> Constants
toConstants sys = mapWithKey toValue (view instances sys)
  where
    toValue :: Name -> TypeName -> Value
    toValue name tyName =
        let ty = TyComponent (Just tyName)
            comp = Component name tyName playerName compartmentName
            playerName = view (boundTo.at name) sys
            compartmentName = view (containedIn.at name) sys
        in Value comp ty