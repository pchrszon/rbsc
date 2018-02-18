{-# LANGUAGE FlexibleContexts #-}


-- | Various utility functions.
module Rbsc.Util
    ( inverseLookup
    , appendIndex
    , topoSort
    ) where


import Control.Lens

import Control.Monad.Except
import Control.Monad.State

import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text, append, pack)


-- | Lookup all keys that have the same value in the map.
inverseLookup :: Eq a => a -> Map k a -> [k]
inverseLookup value = fmap fst . filter ((value ==) . snd) . Map.assocs


-- | Append an 'Integer' to a 'Text'.
appendIndex :: Text -> Integer -> Text
appendIndex base i = base `append` pack (show i)


-- | @topoSort nodes outgoingEdges@ sorts the @nodes@ topologically. If
-- there is an edge from node @x@ to node @y@ (i.e., @x@ depends on @y@),
-- then @y@ comes before @x@ in the returned list.
--
-- If the graph induced by @outgoingEdges@ is cyclic, then the cycle is
-- returned as a @Left@ value.
topoSort :: Eq a => [a] -> (a -> [a]) -> Either (NonEmpty a) [a]
topoSort nodes outgoingEdges =
    over _Right reverse (execStateT (traverse (visit []) nodes) [])
  where
    visit stack node
        | node `elem` stack = throwError (getCycle node stack)
        | otherwise = do
            marked <- get
            unless (node `elem` marked) $ do
                for_ (outgoingEdges node) (visit (node : stack))
                modify (node :)

    getCycle node stack =
        fromList (dropWhile (/= node) (reverse (node : stack)))
