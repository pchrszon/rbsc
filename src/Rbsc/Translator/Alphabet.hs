{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Extraction of component alphabets.
module Rbsc.Translator.Alphabet
    ( Alphabet
    , alphabet
    , stripLocAndKind

    , ModuleAlphabets
    , moduleAlphabets

    , Alphabets
    , componentAlphabets

    , OverrideActions
    , overrideActions
    , isOverrideAction
    ) where


import Control.Lens
import Control.Monad.Except

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as Set


import Rbsc.Data.Action
import Rbsc.Data.Name
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))


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
        Just (Loc (SomeExpr (Literal act _) TyAction) rgn) ->
            return (Just (Loc act rgn, cmdActionKind))
        Just (Loc _ rgn) -> throw rgn NotConstant
        Nothing -> return Nothing


-- | Strip location information and the 'ActionKind's from an 'Alphabet'.
stripLocAndKind :: Alphabet -> Set Action
stripLocAndKind = Set.map (unLoc . fst)


-- | Mapping of module names to their alphabet.
type ModuleAlphabets = Map Name Alphabet


-- | Given a map of component implementations, get the action 'Alphabet' of
-- each module of each component.
moduleAlphabets
    :: MonadError Error m
    => Map ComponentName [TNamedModuleBody Elem]
    -> m (Map ComponentName ModuleAlphabets)
moduleAlphabets = traverse (fmap Map.fromList . traverse fromNamedModuleBody)
  where
    fromNamedModuleBody body =
        (,) (view bodyName body) <$> alphabet (view namedBody body)


-- | Mapping of component names to their alphabet.
type Alphabets = Map ComponentName Alphabet


-- | Given the 'ModuleAlphabets' for each component, get the combined
-- 'Alphabet' of each component.
componentAlphabets :: Map ComponentName ModuleAlphabets -> Alphabets
componentAlphabets = Map.map (Set.unions . Map.elems)


-- | The override actions of a component.
type OverrideActions = Map ComponentName (Set Action)


-- | Get the override actions of all components from the given 'Alphabets'.
overrideActions :: Alphabets -> OverrideActions
overrideActions =
    Map.map (stripLocAndKind . Set.filter (isOverrideAction . snd))


-- | Returns 'True' if the given 'ActionKind' is 'OverrideAction'.
isOverrideAction :: ActionKind -> Bool
isOverrideAction = \case
    OverrideAction _ -> True
    NormalAction     -> False
