{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Top-level definitions.
module Rbsc.Parser.Definition
    ( ErrorOrDef

    , Definition(..)
    , toModel
    ) where


import Control.Lens

import qualified Data.Map.Strict as Map
import           Data.Void

import Text.Megaparsec


import Rbsc.Report.Region (Loc (..))

import Rbsc.Syntax.Untyped


-- | A parse error or a definition.
type ErrorOrDef = Either (ParseError Char Void) Definition


-- | Top-level definitions of a model.
data Definition
    = DefConstant UConstant
    | DefEnumeration Enumeration
    | DefFunction UFunction
    | DefGlobal UVarDecl
    | DefLabel ULabel
    | DefNaturalType NaturalTypeDef
    | DefRoleType RoleTypeDef
    | DefCompartmentType UCompartmentTypeDef
    | DefSystem [Loc Expr]
    | DefImplementation UImplementation
    | DefModule UModule
    | DefCoordinator UCoordinator
    | DefRewardStruct URewardStruct
    deriving (Show)

makePrisms ''Definition


-- | Extract a 'Model' from a list of definitions.
toModel :: [Definition] -> Model
toModel defs = Model
    { modelConstants        = def _DefConstant
    , modelEnumumerations   = def _DefEnumeration
    , modelFunctions        = def _DefFunction
    , modelGlobals          = def _DefGlobal
    , modelLabels           = def _DefLabel
    , modelNaturalTypes     = def _DefNaturalType
    , modelRoleTypes        = def _DefRoleType
    , modelCompartmentTypes = def _DefCompartmentType
    , modelSystem           = concat (def _DefSystem)
    , modelModules          = def _DefModule
    , modelImpls            = def _DefImplementation
    , modelCoordinators     = def _DefCoordinator
    , modelRewardStructs    = mergeRewardStructs (def _DefRewardStruct)
    }
  where
    def p = toListOf (traverse.p) defs


mergeRewardStructs :: [URewardStruct] -> [URewardStruct]
mergeRewardStructs =
    fmap fromTuple . Map.assocs . Map.fromListWith (++) . fmap toTuple
  where
    toTuple RewardStruct{..} = (rsName, rsItems)
    fromTuple = uncurry RewardStruct
