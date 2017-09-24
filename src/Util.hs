-- | Various utility functions.
module Util
    ( inverseLookup
    , appendIndex
    ) where

import           Data.Map  (Map)
import qualified Data.Map  as Map
import           Data.Text (Text, append, pack)


-- | Lookup all keys that have the same value in the map.
inverseLookup :: Eq a => a -> Map k a -> [k]
inverseLookup value = fmap fst . filter ((value ==) . snd) . Map.assocs


-- | Append an 'Integer' to a 'Text'.
appendIndex :: Text -> Integer -> Text
appendIndex base i = base `append` pack (show i)
