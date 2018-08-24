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


import GHC.Exts (IsList (..))


-- | An array.
data Array a = Array Int [a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


instance IsList (Array a) where
    type Item (Array a) = a

    fromList xs = Array (length xs) xs
    toList (Array _ xs) = xs


size :: Array a -> Int
size (Array s _) = s


-- | Safe indexing into an array.
index :: Array a -> Int -> Maybe a
index (Array s arr) i
    | i < 0 || i >= s = Nothing
    | otherwise = Just (arr !! i)
