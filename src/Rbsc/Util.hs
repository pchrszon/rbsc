{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}


-- | Various utility functions.
module Rbsc.Util
    ( renderPretty
    , inverseLookup
    , appendIndex
    , whenIsJust
    , topoSort
    , regions
    ) where


import Control.Lens

import Control.Monad.Except
import Control.Monad.State.Strict

import           Data.Foldable
import           Data.List.NonEmpty                    (NonEmpty, fromList)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, mapMaybe)
import           Data.Set                              (Set, union)
import qualified Data.Set                              as Set
import           Data.Text                             (Text, append, pack)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text


-- | Render a 'Pretty' value to 'Text'.
renderPretty :: Pretty a => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty


-- | Lookup all keys that have the same value in the map.
inverseLookup :: Eq a => a -> Map k a -> [k]
inverseLookup value = fmap fst . filter ((value ==) . snd) . Map.assocs


-- | Append an 'Integer' to a 'Text'.
appendIndex :: Text -> Integer -> Text
appendIndex base i = base `append` pack (show i)


-- | If the given value is 'Just' @x@, then apply @x@ to the given @IO@
-- action.
whenIsJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenIsJust (Just x) m = m x
whenIsJust Nothing  _ = return ()


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


-- | The @regions@ function takes a map of elements where each element has
-- a set of "points" that make up its "region". If the regions of two or
-- more elements are overlapping, they are grouped together and their
-- regions are joined. Elements that have no region (i.e., they are tagged
-- with the empty set) are also grouped together.
regions :: (Ord a, Ord b) => Map a (Set b) -> [(Set b, Set a)]
regions m = catMaybes
    (withoutRegs : evalState (traverse go (Map.keys points)) Set.empty)
  where
    go p = do
        ps <- connected p
        if null ps
            then return Nothing
            else do
                let xs = Set.unions (fmap elemsOf (Set.toList ps))
                return (Just (ps, xs))

    elemsOf p = Map.findWithDefault Set.empty p points

    connected p = do
        visited <- Set.member p <$> get
        if visited
            then return Set.empty
            else do
                modify (Set.insert p)
                Set.insert p . Set.unions <$> traverse connected (neighbors p)

    neighbors p = mapMaybe (neighbor p) edges
    neighbor p (l, r)
        | p == l    = Just r
        | p == r    = Just l
        | otherwise = Nothing

    points = Map.fromListWith union (concatMap toPoints (Map.assocs m))
    toPoints (x, reg) = fmap (, Set.singleton x) (Set.toList reg)

    edges = Set.toList (Set.unions (fmap toEdges (Map.elems m)))
    toEdges reg =
        let reg' = Set.toList reg
        in Set.fromList (zip reg' (tail reg'))

    withoutRegs =
        let xs = Map.keysSet (Map.filter Set.null m)
        in if Set.null xs
               then Nothing
               else Just (Set.empty, xs)
