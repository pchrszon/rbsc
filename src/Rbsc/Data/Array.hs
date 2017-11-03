{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}


-- | Array data type.
module Rbsc.Data.Array
    ( Array
    , fromList
    , toList
    , index
    , length
    ) where


import qualified Data.List as List

import Prelude hiding (length)


-- | An array.
newtype Array a =
    Array [a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | Create a new array from a list.
fromList :: [a] -> Array a
fromList = Array


-- | Convert an array to a list.
toList :: Array a -> [a]
toList (Array xs) = xs


-- | Safe indexing into an array.
index :: Array a -> Int -> Maybe a
index (Array arr) i
    | i < 0 || i > List.length arr - 1 = Nothing
    | otherwise = Just (arr !! i)


-- | Returns the length of an array.
length :: Integral b => Array a -> b
length (Array arr) = List.genericLength arr
