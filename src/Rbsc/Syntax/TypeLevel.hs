{-# LANGUAGE DeriveFunctor #-}


-- | Abstract syntax for component type definitions.
module Rbsc.Syntax.TypeLevel
    ( NaturalTypeDef(..)
    , RoleTypeDef(..)
    , CompartmentTypeDef(..)
    ) where


import Rbsc.Report.Region
import Rbsc.Type


-- | A definition of a natural type.
data NaturalTypeDef a = NaturalTypeDef
    { ntdName  :: !TypeName
    , ntdAnnot :: !a
    } deriving (Functor, Show)


-- | A definition of a role type.
data RoleTypeDef a = RoleTypeDef
    { rtdName    :: !TypeName
    , rtdPlayers :: [(TypeName, Region)]
    , rtdAnnot   :: !a
    } deriving (Functor, Show)


-- | A definition of a compartment type.
data CompartmentTypeDef a = CompartmentTypeDef
    { ctdName  :: !TypeName
    , ctdRoles :: [(TypeName, Region)]
    , ctdAnnot :: !a
    } deriving (Functor, Show)
