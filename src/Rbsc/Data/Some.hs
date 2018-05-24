{-# LANGUAGE GADTs #-}


module Rbsc.Data.Some
    ( Some(..)
    ) where


-- | An existentially quantified value of type @a@.
data Some a where
    Some :: a t -> Some a
