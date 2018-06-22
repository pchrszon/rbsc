{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Extraction of component alphabets.
module Rbsc.Translator.Alphabet
    ( Alphabet
    , alphabet

    , Alphabets
    , alphabets
    ) where


import Control.Monad.Except

import           Data.Map.Strict (Map)
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as Set


import Rbsc.Data.Action
import Rbsc.Data.Name
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed


-- | The action alphabet of a component.
type Alphabet = Set (Loc Action, ActionKind)


-- | Get the set of all actions (i.e., the 'Alphabet') contained in
-- a 'ModuleBody'. If any of the actions cannot be evaluated,
-- a 'NotConstant' error is thrown.
alphabet :: MonadError Error m => TModuleBody Elem -> m Alphabet
alphabet = fmap (Set.fromList . catMaybes) . traverse action . bodyCommands
  where
    action ::
           MonadError Error m
        => TElem (TCommand Elem)
        -> m (Maybe (Loc Action, ActionKind))
    action (Elem Command{..}) = case cmdAction of
        Just (Loc (SomeExpr (Literal act) TyAction) rgn) ->
            return (Just (Loc act rgn, cmdActionKind))
        Just (Loc _ rgn) -> throw rgn NotConstant
        Nothing -> return Nothing


-- | Mapping of component names to their alphabet.
type Alphabets = Map Name Alphabet


-- | Given a map of component implementations, get the action 'Alphabet' of
-- each component.
alphabets :: MonadError Error m => Map Name [TModuleBody Elem] -> m Alphabets
alphabets = traverse (fmap Set.unions . traverse alphabet)
