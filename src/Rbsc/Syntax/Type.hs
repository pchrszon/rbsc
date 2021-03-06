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
    | TyAction
    | TyComponent ComponentTypeSet
    | TyArray expr (Type expr)
    | TyFunc (Type expr) (Type expr)
    deriving (Show, Functor, Foldable, Traversable)
