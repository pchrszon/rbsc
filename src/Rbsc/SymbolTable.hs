-- | Construction of symbol tables.
module Rbsc.SymbolTable
    ( SymbolTable
    ) where


import Data.Map.Strict (Map)

import Rbsc.Type


-- | The symbol table holds the type of each identifier in the model
-- source.
type SymbolTable = Map Name AType
