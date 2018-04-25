{-# LANGUAGE TemplateHaskell #-}


-- | Top-level definitions.
module Rbsc.Parser.Definition
    ( ErrorOrDef

    , Definition(..)
    , toModel
    ) where


import Control.Lens

import Text.Megaparsec


import Rbsc.Report.Region (Loc)

import Rbsc.Syntax.Untyped


-- | A parse error or a definition.
type ErrorOrDef = Either (ParseError Char Dec) Definition


-- | Top-level definitions of a model.
data Definition
    = DefConstant UConstant
    | DefFunction UFunction
    | DefGlobal UGlobal
    | DefNaturalType NaturalTypeDef
    | DefRoleType RoleTypeDef
    | DefCompartmentType UCompartmentTypeDef
    | DefSystem [Loc Expr]
    deriving (Show)

makePrisms ''Definition


-- | Extract a 'Model' from a list of definitions.
toModel :: [Definition] -> Model
toModel defs = Model
    { modelConstants        = def _DefConstant
    , modelFunctions        = def _DefFunction
    , modelGlobals          = def _DefGlobal
    , modelNaturalTypes     = def _DefNaturalType
    , modelRoleTypes        = def _DefRoleType
    , modelCompartmentTypes = def _DefCompartmentType
    , modelSystem           = concat (def _DefSystem)
    }
  where
    def p = toListOf (traverse.p) defs
