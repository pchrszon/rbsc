-- | Instance names and type names.
module Rbsc.Data.Name
    ( Name
    , RoleName
    , TypeName(..)
    , Qualified(..)
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
