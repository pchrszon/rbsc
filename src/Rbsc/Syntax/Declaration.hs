{-# LANGUAGE TemplateHaskell #-}


-- | Top-level declarations.
module Rbsc.Syntax.Declaration
    ( Declaration(..)
    , _DeclNaturalType
    , _DeclRoleType
    , _DeclCompartmentType
    , _DeclSystem
    ) where


import Control.Lens

import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.Constraint
import Rbsc.Syntax.TypeLevel


-- | Top-level declarations of a model.
data Declaration
    = DeclNaturalType NaturalTypeDef
    | DeclRoleType RoleTypeDef
    | DeclCompartmentType CompartmentTypeDef
    | DeclSystem [Loc Constraint]
    deriving (Show)

makePrisms ''Declaration
