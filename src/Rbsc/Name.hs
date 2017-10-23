-- | Instance names and type names.
module Rbsc.Name
    ( Name
    , RoleName
    , TypeName(..)
    ) where


import Data.String
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc


-- | An instance name.
type Name = Text


-- | A 'Name' that is intended to be a role instance name.
type RoleName = Name


-- | The name of a user-defined component type, role type or compartment type.
newtype TypeName = TypeName
    { getTypeName :: Text
    } deriving (Eq, Ord, Show)

instance Pretty TypeName where
    pretty = pretty . getTypeName

instance IsString TypeName where
    fromString = TypeName . fromString
