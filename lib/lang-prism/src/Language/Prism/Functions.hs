{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Built-in functions.
module Language.Prism.Functions
    ( Function(..)
    ) where


import Data.Text.Prettyprint.Doc


-- | Built-in functions.
data Function
    = FuncMin
    | FuncMax
    | FuncFloor
    | FuncCeil
    | FuncPow
    | FuncMod
    | FuncLog
    deriving (Show)

instance Pretty Function where
    pretty = \case
        FuncMin   -> "min"
        FuncMax   -> "max"
        FuncFloor -> "floor"
        FuncCeil  -> "ceil"
        FuncPow   -> "pow"
        FuncMod   -> "mod"
        FuncLog   -> "log"
