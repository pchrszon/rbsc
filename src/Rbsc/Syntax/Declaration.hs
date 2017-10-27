{-# LANGUAGE TemplateHaskell #-}


-- | Top-level declarations.
module Rbsc.Syntax.Declaration
    ( Declaration(..)
    , _DeclConstant
    , _DeclNaturalType
    , _DeclRoleType
    , _DeclCompartmentType
    , _DeclSystem
    ) where


import Control.Lens

import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Constant
import Rbsc.Syntax.Expr.Untyped


-- | Top-level declarations of a model.
data Declaration
    = DeclConstant ConstantDef
    | DeclNaturalType NaturalTypeDef
    | DeclRoleType RoleTypeDef
    | DeclCompartmentType CompartmentTypeDef
    | DeclSystem [Loc Expr]
    deriving (Show)

makePrisms ''Declaration
