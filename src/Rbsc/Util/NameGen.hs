{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Generator for unique names.
module Rbsc.Util.NameGen
    ( HasNameGen(..)

    , NameGen
    , mkNameGen
    , defaultDerive

    , newName
    , newNameFrom
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import           Data.Set   (Set, member)
import           Data.Text  (Text)
import qualified Data.Text  as T

import Rbsc.Util (appendIndex)


-- | A state @t@ that is an instance of 'HasNameGen' carries a 'NameGen'.
class HasNameGen t where
    nameGen :: Lens' t NameGen


-- | A name generator.
--
-- New names are derived from a given name (such as a type name). An index
-- is attached to each generated name to make them unique.
data NameGen = NameGen
    { _counters   :: Map Text Integer
    , _taken      :: Set Text
    , _deriveName :: Text -> Text
    }

instance HasNameGen NameGen where
    nameGen = id

makeLenses ''NameGen


-- | Creates a new 'NameGen' from a given name derivation function and
-- a 'Set' of names that are already taken.
mkNameGen :: (Text -> Text) -> Set Text -> NameGen
mkNameGen d ts = NameGen {_counters = Map.empty, _taken = ts, _deriveName = d}


-- | The default name derivation function.
--
-- The given name is returned as is. In case an empty 'Text' is given, the
-- derived name is @"x"@.
defaultDerive :: Text -> Text
defaultDerive name
    | T.null name = "x"
    | otherwise = name


-- | Creates a new name.
newName :: (MonadState s m, HasNameGen s) => m Text
newName = newNameFrom "x"


-- | Creates a new name derived from the given name.
newNameFrom :: (MonadState s m, HasNameGen s) => Text -> m Text
newNameFrom name = do
    dn <- use $ nameGen.deriveName
    ts <- use $ nameGen.taken

    let base = dn name

    i <- fromMaybe 0 <$> use (nameGen.counters.at base)
    let i' = findUnusedIndex ts base i

    nameGen.counters.at base .= Just (i' + 1)

    return $ appendIndex base i'


-- | Find the smallest index greater or equal i that is unused.
findUnusedIndex :: Set Text -> Text -> Integer -> Integer
findUnusedIndex ts base i
    | name `member` ts = findUnusedIndex ts base (i + 1)
    | otherwise = i
  where
    name = appendIndex base i
