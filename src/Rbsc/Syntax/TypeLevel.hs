{-# LANGUAGE DeriveFunctor #-}


-- | Abstract syntax for component type definitions.
module Rbsc.Syntax.TypeLevel
    ( NaturalTypeDef(..)
    , RoleTypeDef(..)
    , CompartmentTypeDef(..)
    ) where


import Rbsc.Type


-- | A definition of a natural type.
data NaturalTypeDef l = NaturalTypeDef
    { ntdName  :: TypeName
    , ntdAnnot :: !l
    } deriving (Functor, Show)


-- | A definition of a role type.
data RoleTypeDef l = RoleTypeDef
    { rtdName    :: !TypeName
    , rtdPlayers :: [TypeName]
    , rtdAnnot   :: !l
    } deriving (Functor, Show)


-- | A definition of a compartment type.
data CompartmentTypeDef l = CompartmentTypeDef
    { ctdName  :: !TypeName
    , ctdRoles :: [TypeName]
    , ctdAnnot :: !l
    } deriving (Functor, Show)
