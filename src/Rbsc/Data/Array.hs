{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}


-- | Array data type.
module Rbsc.Data.Array
    ( Array
    , fromList
    , size
    , index
    ) where


import qualified Data.Array as Array

import GHC.Exts (IsList (..))


-- | An array.
newtype Array a = Array (Array.Array Int a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


instance IsList (Array a) where
    type Item (Array a) = a

    fromList xs = Array (Array.listArray (0, length xs - 1) xs)
    toList (Array arr) = Array.elems arr


size :: Array a -> Int
size (Array arr) = let (_, upper) = Array.bounds arr in upper + 1


-- | Safe indexing into an array.
index :: Array a -> Int -> Maybe a
index (Array arr) i
    | i < lower || i > upper = Nothing
    | otherwise = Just (arr Array.! i)
  where
    (lower, upper) = Array.bounds arr
