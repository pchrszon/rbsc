{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}


-- | Abstract syntax of types.
module Rbsc.Syntax.Type
    ( Type(..)
    ) where


import Rbsc.Data.ComponentType


-- | Abstract syntax of a type.
data Type expr
    = TyBool
    | TyInt
    | TyDouble
    | TyComponent ComponentTypeSet
    | TyArray (expr, expr) (Type expr)
    deriving (Show, Functor, Foldable, Traversable)
