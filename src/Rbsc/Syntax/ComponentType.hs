-- | Abstract syntax for component type definitions.
module Rbsc.Syntax.ComponentType
    ( NaturalTypeDef(..)
    , RoleTypeDef(..)
    , CompartmentTypeDef(..)
    ) where


import Rbsc.Name
import Rbsc.Report.Region


-- | A definition of a natural type.
newtype NaturalTypeDef = NaturalTypeDef
    { ntdName  :: Loc TypeName
    } deriving (Show)


-- | A definition of a role type.
data RoleTypeDef = RoleTypeDef
    { rtdName    :: Loc TypeName
    , rtdPlayers :: [Loc TypeName]
    } deriving (Show)


-- | A definition of a compartment type.
data CompartmentTypeDef = CompartmentTypeDef
    { ctdName  :: Loc TypeName
    , ctdRoles :: [Loc TypeName]
    } deriving (Show)
