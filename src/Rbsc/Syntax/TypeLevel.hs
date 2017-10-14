-- | Abstract syntax for component type definitions.
module Rbsc.Syntax.TypeLevel
    ( NaturalTypeDef(..)
    , RoleTypeDef(..)
    , CompartmentTypeDef(..)
    ) where


import Rbsc.Report.Region
import Rbsc.Type


-- | A definition of a natural type.
newtype NaturalTypeDef = NaturalTypeDef
    { ntdName  :: Ann TypeName Region
    } deriving (Show)


-- | A definition of a role type.
data RoleTypeDef = RoleTypeDef
    { rtdName    :: Ann TypeName Region
    , rtdPlayers :: [Ann TypeName Region]
    } deriving (Show)


-- | A definition of a compartment type.
data CompartmentTypeDef = CompartmentTypeDef
    { ctdName  :: Ann TypeName Region
    , ctdRoles :: [Ann TypeName Region]
    } deriving (Show)
