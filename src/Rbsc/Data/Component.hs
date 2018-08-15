{-# LANGUAGE TemplateHaskell #-}


-- | Internal representation of component instances.
module Rbsc.Data.Component
    ( Component(..)
    , compName
    , compTypeName
    , compBoundTo
    , compContainedIn
    ) where


import Control.Lens

import Rbsc.Data.Name


-- | A component instance (either a natural, a role or a compartment).
data Component = Component
    { _compName        :: !ComponentName
    , _compTypeName    :: !TypeName
    , _compBoundTo     :: Maybe ComponentName
    , _compContainedIn :: Maybe ComponentName
    } deriving (Eq, Show)


makeLenses ''Component
