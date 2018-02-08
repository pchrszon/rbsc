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
    = DefConstant UConstantDef
    | DefNaturalType NaturalTypeDef
    | DefRoleType RoleTypeDef
    | DefCompartmentType CompartmentTypeDef
    | DefSystem [Loc Expr]
    deriving (Show)

makePrisms ''Definition


toModel :: [Definition] -> UModel
toModel defs = Model
    { constants        = def _DefConstant
    , naturalTypes     = def _DefNaturalType
    , roleTypes        = def _DefRoleType
    , compartmentTypes = def _DefCompartmentType
    , system           = concat (def _DefSystem)
    }
  where
    def p = toListOf (traverse.p) defs
