{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Configuration options.
module Rbsc.Config
    ( RecursionDepth(..)
    , HasRecursionDepth(..)
    ) where


import Control.Lens


-- | The maximal recursion depth.
newtype RecursionDepth = RecursionDepth Int deriving (Eq, Ord, Read, Show, Num)


class HasRecursionDepth a where
    recursionDepth :: Lens' a RecursionDepth

instance HasRecursionDepth RecursionDepth where
    recursionDepth = id
