{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}


-- | Abstract syntax related to quantification.
module Rbsc.Syntax.Quantification
    ( Quantifier(..)
    , QuantifiedType(..)
    ) where


-- | A quantifier.
data Quantifier
    = Forall
    | Exists
    deriving (Eq, Show)


-- | The type of the variable introduced by a quantifier.
data QuantifiedType comp expr
    = QdTypeComponent comp
    | QdTypeInt (expr, expr)
    deriving (Show, Functor, Foldable, Traversable)
