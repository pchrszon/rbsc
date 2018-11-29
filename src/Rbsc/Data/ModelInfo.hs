{-# LANGUAGE TemplateHaskell #-}


-- | The 'ModelInfo' provides information about the identifiers and types
-- defined in the model.
module Rbsc.Data.ModelInfo
    ( ModelInfo(..)
    , emptyModelInfo

    , componentTypes
    , typeSets
    , symbolTable
    , constants
    , methods

    , ComponentTypes
    , SymbolTable
    , Constants
    , Methods
    ) where


import Control.Lens


import Rbsc.Data.ComponentType
import Rbsc.Data.Type

import Rbsc.Syntax.Typed.Expr


-- | The @ModelInfo@ holds the types of identifiers, the user-defined types
-- and the values of constants defined in the model.
data ModelInfo = ModelInfo
    { _miComponentTypes :: !ComponentTypes
    , _miTypeSets       :: !TypeSets
    , _miSymbolTable    :: !SymbolTable
    , _miRangeTable     :: !RangeTable
    , _miConstants      :: !Constants
    , _miMethods        :: !Methods
    } deriving (Show)

makeLenses ''ModelInfo

instance HasComponentTypes ModelInfo where
    componentTypes = miComponentTypes

instance HasTypeSets ModelInfo where
    typeSets = miTypeSets

instance HasSymbolTable ModelInfo where
    symbolTable = miSymbolTable

instance HasRangeTable ModelInfo where
    rangeTable = miRangeTable

instance HasConstants ModelInfo where
    constants = miConstants

instance HasMethods ModelInfo where
    methods = miMethods


-- | Empty @ModelInfo@.
emptyModelInfo :: ModelInfo
emptyModelInfo = ModelInfo mempty mempty mempty mempty mempty mempty
