-- | Representation of visibility of symbols.
module Rbsc.Data.Scope
    ( Scope(..)
    , ScopedName(..)
    ) where


import Rbsc.Data.Name


-- | The scope defines the visibility of symbols.
data Scope
    = Global
    | Local !TypeName
    deriving (Eq, Ord, Show)


-- | A 'Name' with its 'Scope'.
data ScopedName = ScopedName !Scope !Name deriving (Eq, Ord, Show)
