{-# LANGUAGE TypeOperators #-}


-- | The 'ModelInfo' provides information about the identifiers and types
-- defined in the model.
module Rbsc.Data.ModelInfo
    ( ModelInfo
    , emptyModelInfo

    , componentTypes
    , typeSets
    , symbolTable
    , rangeTable
    , constants
    , methods

    , ComponentTypes
    , TypeSets
    , SymbolTable
    , RangeTable
    , Constants
    , Methods
    ) where


import Rbsc.Data.ComponentType
import Rbsc.Data.Field
import Rbsc.Data.Type

import Rbsc.Syntax.Typed.Expr


-- | The @ModelInfo@ holds information extracted from the model, including
-- the types of all identifiers, user-defined types and the values of all
-- constants defined in the model.
type ModelInfo =
    ComponentTypes :&:
    TypeSets :&:
    SymbolTable :&:
    RangeTable :&:
    Constants :&:
    Methods


-- | Empty @ModelInfo@.
emptyModelInfo :: ModelInfo
emptyModelInfo = mempty
