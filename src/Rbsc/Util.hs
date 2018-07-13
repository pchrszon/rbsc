{-# LANGUAGE FlexibleContexts #-}


-- | Various utility functions.
module Rbsc.Util
    ( renderPretty
    , inverseLookup
    , appendIndex
    , toMaybe
    , whenIsJust
    , topoSort
    ) where


import Control.Lens

import Control.Monad.Except
import Control.Monad.State.Strict

import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty, fromList)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text, append, pack)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text


-- | Render a 'Pretty' value to 'Text'.
renderPretty :: Pretty a => a -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty


-- | Lookup all keys that have the same value in the map.
inverseLookup :: Eq a => a -> Map k a -> [k]
inverseLookup value = fmap fst . filter ((value ==) . snd) . Map.assocs


-- | Append an 'Integer' to a 'Text'.
appendIndex :: Text -> Integer -> Text
appendIndex base i = base `append` pack (show i)


-- | @toMaybe x b@ returns @Just x@ if @b@ is @True@, else @Nothing@.
toMaybe :: a -> Bool -> Maybe a
toMaybe x True  = Just x
toMaybe _ False = Nothing


-- | If the given value is 'Just' @x@, then apply @x@ to the given @IO@
-- action.
whenIsJust :: Maybe a -> (a -> IO ()) -> IO ()
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
