{-# LANGUAGE TemplateHaskell #-}


module Rbsc.Data.Info
    ( Info(..)
    , modelInfo
    ) where


import Control.Lens


import Rbsc.Config

import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Syntax.Typed.Expr


data Info = Info
    { _modelInfo          :: ModelInfo
    , _confRecursionDepth :: RecursionDepth
    }

makeLenses ''Info

instance HasRecursionDepth Info where
    recursionDepth = confRecursionDepth

instance HasComponentTypes Info where
    componentTypes = modelInfo.componentTypes

instance HasSymbolTable Info where
    symbolTable = modelInfo.symbolTable

instance HasRangeTable Info where
    rangeTable = modelInfo.rangeTable

instance HasConstants Info where
    constants = modelInfo.constants

instance HasMethods Info where
    methods = modelInfo.methods
