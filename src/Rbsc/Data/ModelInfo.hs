{-# LANGUAGE TemplateHaskell #-}


-- | The 'ModelInfo' provides information about the identifiers and types
-- defined in the model.
module Rbsc.Data.ModelInfo
    ( ModelInfo(..)

    , ComponentType(..)
    , SymbolTable
    , Constants

    , componentTypes
    , symbolTable
    , constants
    ) where


import Control.Lens


import Rbsc.Data.ComponentType
import Rbsc.Data.Type

import Rbsc.Syntax.Expr.Typed


-- | The @ModelInfo@ holds the types of identifiers, the user-defined types
-- and the values of constants defined in the model.
data ModelInfo = ModelInfo
    { _componentTypes :: !ComponentType
    , _symbolTable    :: !SymbolTable
    , _constants      :: !Constants
    } deriving (Show)

makeLenses ''ModelInfo
