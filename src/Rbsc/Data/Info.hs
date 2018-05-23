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

import Rbsc.Syntax.Expr.Typed


data Info = Info
    { _modelInfo :: ModelInfo
    , _confRecursionDepth :: RecursionDepth
    }

makeLenses ''Info

instance HasRecursionDepth Info where
    recursionDepth = confRecursionDepth

instance HasComponentTypes Info where
    componentTypes = modelInfo.componentTypes

instance HasSymbolTable Info where
    symbolTable = modelInfo.symbolTable

instance HasConstants Info where
    constants = modelInfo.constants
