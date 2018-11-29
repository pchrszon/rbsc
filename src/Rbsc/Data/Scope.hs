{-# LANGUAGE LambdaCase #-}


-- | Representation of visibility of symbols.
module Rbsc.Data.Scope
    ( Scope(..)
    , fromMaybeTypeName
    , ScopedName(..)
    ) where


import Rbsc.Data.Name


-- | The scope defines the visibility of symbols.
data Scope
    = Global
    | Local !TypeName
    deriving (Eq, Ord, Show)


-- | Construct a 'Scope' from a 'Maybe' 'TypeName'. If 'Nothing' is given,
-- the @Scope@ is 'Global'.
fromMaybeTypeName :: Maybe TypeName -> Scope
fromMaybeTypeName = \case
    Just tyName -> Local tyName
    Nothing     -> Global


-- | A 'Name' with its 'Scope'.
data ScopedName = ScopedName !Scope !Name deriving (Eq, Ord, Show)
