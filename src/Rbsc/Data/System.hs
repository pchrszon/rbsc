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

    , toComponents
    ) where


import Control.Lens
import Data.Map.Strict           (Map, assocs)
import Data.Text.Prettyprint.Doc


import Rbsc.Data.Component
import Rbsc.Data.Name

import Rbsc.Util


-- | A system instance that assigns a type to each component, role and
-- compartment. Also specifies role bindings and compartment membership.
--
-- A system instance may be incomplete, i.e., roles may be unbound and
-- compartments may be missing certain role instances.
data System = System
    { _instances   :: Map ComponentName TypeName
    , _boundTo     :: Map RoleName ComponentName
    , _containedIn :: Map RoleName ComponentName
    } deriving (Eq, Show)

makeLenses ''System

instance Pretty System where
    pretty sys =
        sep (fmap (<> semi) (instanceDocs ++ bindingDocs ++ containmentDocs))
      where
        instanceDocs = fmap prettyInstance (view (instances.to assocs) sys)
        bindingDocs = fmap prettyBinding (view (boundTo.to assocs) sys)
        containmentDocs =
            fmap prettyContained (view (containedIn.to assocs) sys)

prettyInstance :: (ComponentName, TypeName) -> Doc ann
prettyInstance (name, tyName) = pretty name <+> colon <+> pretty tyName

prettyBinding :: (RoleName, ComponentName) -> Doc ann
prettyBinding (roleName, playerName) =
    pretty roleName <+> "boundto" <+> pretty playerName

prettyContained :: (RoleName, ComponentName) -> Doc ann
prettyContained (roleName, compartmentName) =
    pretty roleName <+> "in" <+> pretty compartmentName


-- | Get all instances that have the given type.
instancesOfType :: TypeName -> System -> [ComponentName]
instancesOfType tyName = inverseLookup tyName . view instances


-- | Get all roles that are bound to a given player.
boundRoles :: ComponentName -> System -> [RoleName]
boundRoles name = inverseLookup name . view boundTo


-- | Get all roles contained in a given compartment.
containedRoles :: ComponentName -> System -> [RoleName]
containedRoles name = inverseLookup name . view containedIn


-- | Get all 'Component's that comprise the 'System'.
toComponents :: System -> [Component]
toComponents sys = fmap toComponent (view (instances.to assocs) sys)
  where
    toComponent (name, tyName) = Component
        { _compName        = name
        , _compTypeName    = tyName
        , _compBoundTo     = view (boundTo.at name) sys
        , _compContainedIn = view (containedIn.at name) sys
        }
