{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Configuration options.
module Rbsc.Config
    ( RecursionDepth(..)
    , recursionDepth
    ) where


import Control.Lens


import Rbsc.Data.Field


-- | The maximal recursion depth.
newtype RecursionDepth = RecursionDepth Int deriving (Eq, Ord, Read, Show, Num)


-- | A 'Lens' for accessing the 'RecursionDepth'.
recursionDepth :: Has RecursionDepth r => Lens' r RecursionDepth
recursionDepth = field
