{-# LANGUAGE TemplateHaskell #-}


-- | The 'ModelInfo' provides information about the identifiers and types
-- defined in the model.
module Rbsc.Data.ModelInfo
    ( ModelInfo(..)
    , emptyModelInfo

    , componentTypes
    , symbolTable
    , constants

    , ComponentTypes
    , SymbolTable
    , Constants
    ) where


import Control.Lens


import Rbsc.Data.ComponentType
import Rbsc.Data.Type

import Rbsc.Syntax.Expr.Typed


-- | The @ModelInfo@ holds the types of identifiers, the user-defined types
-- and the values of constants defined in the model.
data ModelInfo = ModelInfo
    { _miComponentTypes :: !ComponentTypes
    , _miSymbolTable    :: !SymbolTable
    , _miConstants      :: !Constants
    } deriving (Show)

makeLenses ''ModelInfo

instance HasComponentTypes ModelInfo where
    componentTypes = miComponentTypes

instance HasSymbolTable ModelInfo where
    symbolTable = miSymbolTable

instance HasConstants ModelInfo where
    constants = miConstants


-- | Empty @ModelInfo@.
emptyModelInfo :: ModelInfo
emptyModelInfo = ModelInfo mempty mempty mempty
