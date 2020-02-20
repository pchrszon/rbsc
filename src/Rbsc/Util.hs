{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}


-- | Various utility functions.
module Rbsc.Util
    ( renderPretty
    , inverseLookup
    , inverseIndex
    , appendIndex
    , mapAnnotate
    , mapAnnotateA
    , whenIsJust
    , withConstants
    , topoSort

    , Graph
    , connectedComponents
    , reachable
    ) where


import Control.Lens

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import           Data.Foldable
import           Data.List.NonEmpty                    (NonEmpty, fromList)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes)
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Data.Text                             (Text, append, pack)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text


import Rbsc.Data.Field
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Typed      (LSomeExpr)
import Rbsc.Syntax.Typed.Expr


-- | Render a 'Pretty' value to 'Text'.
renderPretty :: Pretty a => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty


-- | Lookup all keys that have the same value in the map.
inverseLookup :: Eq a => a -> Map k a -> [k]
inverseLookup value = fmap fst . filter ((value ==) . snd) . Map.assocs


-- | Given a list of key-value pairs, swap keys and values and put them in a
-- 'Map'.
inverseIndex :: (Ord a, Ord b) => [(a, Set b)] -> Map b (Set a)
inverseIndex = Map.fromListWith Set.union . concatMap pairs
  where
    pairs (x, ys) = zip (Set.toList ys) (repeat (Set.singleton x))


-- | Append an 'Integer' to a 'Text'.
appendIndex :: Text -> Integer -> Text
appendIndex base i = base `append` pack (show i)


-- | @mapAnnotate f xs@ applies the function @f@ to each element in @xs@ and
-- pairs each element with the result.
mapAnnotate :: (a -> b) -> [a] -> [(a, b)]
mapAnnotate f = fmap (\x -> (x, f x))


-- | Effectful version of 'mapAnnotate'.
mapAnnotateA :: Applicative f => (a -> f b) -> [a] -> f [(a, b)]
mapAnnotateA f = traverse (\x -> (,) x <$> f x)


-- | If the given value is 'Just' @x@, then apply @x@ to the given @IO@
-- action.
whenIsJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenIsJust (Just x) m = m x
whenIsJust Nothing  _ = return ()


-- | Locally add a list of constants to the constant table and the symbol table.
withConstants
    :: (MonadReader r m, Has SymbolTable r, Has Constants r)
    => [(Name, LSomeExpr)]
    -> m a
    -> m a
withConstants args = local
    ( over symbolTable (Map.union argSymbols)
    . over constants   (Map.union argConsts)
    )
  where
    argConsts  = Map.fromList (fmap mkConst args)
    argSymbols = Map.fromList (fmap mkSymbol args)

    mkConst (name, e) = (name, unLoc e)
    mkSymbol (name, Loc (SomeExpr _ ty) _) = (ScopedName Global name, Some ty)


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


-- | A graph with node type @a@.
type Graph a = Map a (Set a)


-- | Get the list of connected components of the graph. The graph should be
-- undirected, i.e., for each edges there must exist an inverse edge in the
-- graph.
connectedComponents :: Ord a => Graph a -> [Set a]
connectedComponents g =
    catMaybes (evalState (traverse toComponent (Map.keys g)) Set.empty)
  where
    toComponent x = do
        visited <- get
        if x `Set.member` visited
            then return Nothing
            else do
                let comp = reachable g x
                modify (Set.union comp)
                return (Just comp)


-- | The set of all nodes that are reachable from the given starting node.
reachable :: Ord a => Graph a -> a -> Set a
reachable g root = execState (visit root) Set.empty
  where
    visit x = do
        visited <- get
        unless (x `Set.member` visited) $ do
            modify (Set.insert x)
            traverse_ visit (Set.toList (Map.findWithDefault Set.empty x g))
