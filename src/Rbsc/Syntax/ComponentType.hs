-- | Abstract syntax for component type definitions.
module Rbsc.Syntax.ComponentType
    ( NaturalTypeDef(..)
    , RoleTypeDef(..)
    , CompartmentTypeDef(..)
    , MultiRole(..)
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
data CompartmentTypeDef expr = CompartmentTypeDef
    { ctdName  :: Loc TypeName
    , ctdRoles :: [[MultiRole expr]]
    } deriving (Show)

instance Eq (CompartmentTypeDef expr) where
    (==) = (==) `on` ctdName

instance Ord (CompartmentTypeDef expr) where
    compare = comparing ctdName


-- | A reference to a role type with optional cardinalities.
data MultiRole expr = MultiRole
    { mrName   :: Loc TypeName
    , mrBounds :: Maybe (expr, expr)
    } deriving (Show)
