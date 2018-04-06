{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}


-- | Abstract syntax related to quantification.
module Rbsc.Syntax.Quantification
    ( QuantifiedType(..)
    ) where


-- | The type of the variable introduced by a quantifier.
data QuantifiedType comp expr
    = QdTypeComponent comp
    | QdTypeInt (expr, expr)
    deriving (Show, Functor, Foldable, Traversable)
