{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}


-- | Array data type.
module Rbsc.Data.Array
    ( Array
    , fromList
    , toList
    , bounds
    , index
    ) where


-- | An array.
data Array a = Array (Int, Int) [a]
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | Create a new array from a list.
fromList :: [a] -> Array a
fromList xs = Array (0, length xs - 1) xs


-- | Convert an array to a list.
toList :: Array a -> [a]
toList (Array _ xs) = xs


bounds :: Array a -> (Int, Int)
bounds (Array b _) = b


-- | Safe indexing into an array.
index :: Array a -> Int -> Maybe a
index (Array (lower, upper) arr) i
    | i < lower || i > upper = Nothing
    | otherwise = Just (arr !! (i - lower))
