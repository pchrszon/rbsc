{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}


-- | An append-only Bag data structure with O(1) cons and append
-- operations.
module Rbsc.Data.Bag
    ( Bag
    , empty
    , singleton
    , cons
    , fromList
    , toList
    ) where


import GHC.Exts (IsList (..))


-- | A bag holding elements of type @a@.
data Bag a
    = Empty
    | One a
    | Two (Bag a) (Bag a)
    | Many [a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance IsList (Bag a) where
    type Item (Bag a) = a

    fromList   = Many
    toList bag = bagToList bag []

instance Semigroup (Bag a) where
    Empty <> r = r
    l <> Empty = l
    l <> r     = Two l r

instance Monoid (Bag a) where
    mempty  = Empty
    mappend = (<>)


bagToList :: Bag a -> [a] -> [a]
bagToList bag xs = case bag of
    Empty    -> xs
    One x    -> x:xs
    Two l r  -> bagToList l (bagToList r xs)
    Many xs' -> xs' ++ xs


-- | The empty 'Bag'.
empty :: Bag a
empty = Empty


-- | A 'Bag' containing only the given element.
singleton :: a -> Bag a
singleton = One


-- | Preprend an element to a 'Bag'.
cons :: a -> Bag a -> Bag a
cons x = \case
    Empty -> One x
    y     -> Two (One x) y
