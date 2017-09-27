{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Rbsc.System
    ( System(..)
    , instances
    , boundTo
    , containedIn

    , instancesOfType
    , boundRoles
    , containedRoles
    ) where


import           Control.Lens
import           Data.Map.Strict           (Map, assocs)
import qualified Data.Map.Strict           as Map
import           Data.Text.Prettyprint.Doc

import Rbsc.Type
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
    pretty sys = sep (punctuate comma (instanceDocs ++ bindingDocs))
      where
        instanceDocs =
            fmap (prettyInstance sys) (view (instances.to assocs) sys)
        bindingDocs =
            fmap prettyBinding .
            filter (not . null . snd) .
            fmap (\i -> (i, boundRoles i sys)) .
            view (instances.to Map.keys) $ sys

prettyInstance :: System -> (Name, TypeName) -> Doc ann
prettyInstance sys (name, tyName) =
    pretty name <+>
    colon <+>
    pretty tyName <>
    if null contained
        then emptyDoc
        else space <> hang 0 (parens (commaSep contained))
  where
    contained = containedRoles name sys

prettyBinding :: (Name, [RoleName]) -> Doc ann
prettyBinding (name, roleNames) =
    pretty name <+> "boundto" <+> hang 0 (parens (commaSep roleNames))

commaSep :: Pretty a => [a] -> Doc ann
commaSep = sep . punctuate comma . fmap pretty


-- | Get all instances that have the given type.
instancesOfType :: TypeName -> System -> [Name]
instancesOfType tyName = inverseLookup tyName . view instances


-- | Get all roles that are bound to a given player.
boundRoles :: Name -> System -> [RoleName]
boundRoles name = inverseLookup name . view boundTo


-- | Get all roles contained in a given compartment.
containedRoles :: Name -> System -> [RoleName]
containedRoles name = inverseLookup name . view containedIn
