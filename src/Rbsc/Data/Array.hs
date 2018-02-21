{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}


-- | Array data type.
module Rbsc.Data.Array
    ( Array
    , fromList
    , bounds
    , index
    ) where


import GHC.Exts (IsList (..))


-- | An array.
data Array a = Array (Int, Int) [a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


instance IsList (Array a) where
    type Item (Array a) = a

    fromList xs = Array (0, length xs - 1) xs
    toList (Array _ xs) = xs


bounds :: Array a -> (Int, Int)
bounds (Array b _) = b


-- | Safe indexing into an array.
index :: Array a -> Int -> Maybe a
index (Array (lower, upper) arr) i
    | i < lower || i > upper = Nothing
    | otherwise = Just (arr !! (i - lower))
