{-# LANGUAGE OverloadedStrings #-}


-- | Instance names and type names.
module Rbsc.Data.Name
    ( Name
    , RoleName
    , TypeName(..)
    , ComponentName(..)
    , componentName
    , Qualified(..)
    ) where


import Data.String
import Data.Text                 (Text, pack)
import Data.Text.Prettyprint.Doc


-- | An instance name.
type Name = Text


data ComponentName = ComponentName !Name (Maybe Int) deriving (Eq, Ord, Show)

instance Pretty ComponentName where
    pretty (ComponentName name mIdx) = case mIdx of
        Just idx -> pretty name <> brackets (pretty idx)
        Nothing  -> pretty name

instance IsString ComponentName where
    fromString s = ComponentName (fromString s) Nothing


componentName :: ComponentName -> Name
componentName (ComponentName name mIdx) = case mIdx of
    Just idx -> name <> "[" <> pack (show idx) <> "]"
    Nothing  -> name


-- | A 'ComponentName' that is intended to be a role instance name.
type RoleName = ComponentName


-- | The name of a user-defined component type, role type or compartment type.
newtype TypeName = TypeName
    { getTypeName :: Text
    } deriving (Eq, Ord)

instance Show TypeName where
    showsPrec p (TypeName n) = showsPrec p n

instance Pretty TypeName where
    pretty = pretty . getTypeName

instance IsString TypeName where
    fromString = TypeName . fromString


-- | A fully qualified name.
data Qualified
    = QlName !Name
    | QlMember Qualified !Name
    | QlIndex Qualified !Int
    deriving (Eq, Ord, Show)
