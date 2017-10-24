{-# LANGUAGE TemplateHaskell #-}


-- | Internal representation of component instances.
module Rbsc.Component
    ( Component(..)
    , compName
    , compTypeName
    , compBoundTo
    , compContainedIn
    ) where


import Control.Lens

import Rbsc.Name


-- | A component instance (either a natural, a role or a compartment).
data Component = Component
    { _compName        :: !Name
    , _compTypeName    :: !TypeName
    , _compBoundTo     :: Maybe Name
    , _compContainedIn :: Maybe Name
    } deriving (Eq, Show)


makeLenses ''Component
