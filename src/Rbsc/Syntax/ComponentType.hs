-- | Abstract syntax for component type definitions.
module Rbsc.Syntax.ComponentType
    ( NaturalTypeDef(..)
    , RoleTypeDef(..)
    , CompartmentTypeDef(..)
    ) where


import Data.Function
import Data.Ord


import Rbsc.Data.Name
import Rbsc.Report.Region


-- | A definition of a natural type.
newtype NaturalTypeDef = NaturalTypeDef
    { ntdName  :: Loc TypeName
    } deriving (Eq, Ord, Show)


-- | A definition of a role type.
data RoleTypeDef = RoleTypeDef
    { rtdName    :: Loc TypeName
    , rtdPlayers :: [Loc TypeName]
    } deriving (Show)

instance Eq RoleTypeDef where
    (==) = (==) `on` rtdName

instance Ord RoleTypeDef where
    compare = comparing rtdName


-- | A definition of a compartment type.
data CompartmentTypeDef = CompartmentTypeDef
    { ctdName  :: Loc TypeName
    , ctdRoles :: [Loc TypeName]
    } deriving (Show)

instance Eq CompartmentTypeDef where
    (==) = (==) `on` ctdName

instance Ord CompartmentTypeDef where
    compare = comparing ctdName
