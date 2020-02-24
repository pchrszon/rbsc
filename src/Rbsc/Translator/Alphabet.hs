{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Extraction of component alphabets.
module Rbsc.Translator.Alphabet
    ( Alphabet
    , alphabet
    , stripActionInfo

    , ModuleAlphabets
    , moduleAlphabets

    , Alphabets
    , componentAlphabets

    , coordinatorActions

    , OverrideActions
    , overrideActions

    , checkActionIndices
    , checkSynchronizations
    ) where


import Control.Lens
import Control.Monad.Except

import           Data.Foldable
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
import Rbsc.Report.Result

import Rbsc.Syntax.Typed hiding (Type (..))


-- | The action alphabet of a component.
type Alphabet = Set ActionInfo


-- | Get the set of all actions (i.e., the 'Alphabet') contained in
-- a 'ModuleBody'. If any of the actions cannot be evaluated,
-- a 'NotConstant' error is thrown.
alphabet :: MonadError Error m => TModuleBody Elem -> m Alphabet
alphabet = fmap (Set.fromList . catMaybes) . traverse action . bodyCommands
  where
    action
        :: MonadError Error m => TElem (TCommand Elem) -> m (Maybe ActionInfo)
    action (Elem Command {..}) = case cmdAction of
        Just (Loc (SomeExpr (Literal act _) TyAction) rgn) -> return
            (Just (ActionInfo (Loc act rgn) cmdActionKind cmdActionIntent))
        Just (Loc _ rgn) -> throw rgn NotConstant
        Nothing          -> return Nothing


-- | Get the set of all actions used by a 'Coordinator'. If any of the actions
-- cannot be evaluated, a 'NotConstant' error is thrown.
coordinatorActions :: MonadError Error m => TCoordinator Elem -> m (Set Action)
coordinatorActions =
    fmap (Set.fromList . catMaybes) . traverse action . coordCommands
  where
    action
        :: MonadError Error m => TElem (TCoordCommand Elem) -> m (Maybe Action)
    action (Elem CoordCommand {..}) = case coordAction of
        Just (Loc (SomeExpr (Literal act _) TyAction) _) -> return (Just act)
        Just (Loc _ rgn) -> throw rgn NotConstant
        Nothing          -> return Nothing


-- | Strip locations, 'ActionKind's and 'ActionIntent's from an 'Alphabet'.
stripActionInfo :: Alphabet -> Set Action
stripActionInfo = Set.map (unLoc . actionName)


-- | Mapping of module names to their alphabet.
type ModuleAlphabets = Map Name Alphabet


-- | Given a map of component implementations, get the action 'Alphabet' of
-- each module of each component.
moduleAlphabets
    :: MonadError Error m
    => Map ComponentName [TModuleInstance Elem]
    -> m (Map ComponentName ModuleAlphabets)
moduleAlphabets = traverse (fmap Map.fromList . traverse fromModuleInstance)
  where
    fromModuleInstance inst =
        (,) (view miName inst) <$> alphabet (view miBody inst)


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
overrideActions = Map.map (stripActionInfo . Set.filter isOverrideAction)


-- | Check if the number of indices is consistent for each action. If not,
-- an 'InconsistentActionIndices' warning is reported.
checkActionIndices :: Map ComponentName ModuleAlphabets -> Result ()
checkActionIndices mas = for_ (Map.elems actIndices) $ \case
    ((n1, rgn1) : (n2, rgn2) : _) ->
        warn (InconsistentActionIndices rgn1 n1 rgn2 n2)
    _ -> return ()
  where
    actIndices :: Map Action [(Int, Region)]
    actIndices = Map.map Map.assocs . Map.fromListWith Map.union $
        fmap (toElem . numberOfIndices) (actions mas)

    actions =
        Set.toList
            . Set.map actionName
            . Set.unions
            . concatMap Map.elems
            . Map.elems

    toElem (Loc act rgn, n) = (act, Map.singleton n rgn)


numberOfIndices :: Loc Action -> (Loc Action, Int)
numberOfIndices (Loc act rgn) = go 0 act
  where
    go !i = \case
        IndexedAction act' _ -> go (succ i) act'
        act'                 -> (Loc act' rgn, i)


-- | Check if there are actions that do not synchronize with any other
-- module.
checkSynchronizations
    :: Map ComponentName ModuleAlphabets -> Set Action -> Result ()
checkSynchronizations mas coordActions = for_ (Map.assocs syncs) $ \case
    (act, [_]) | Set.notMember (unLoc act) coordActions ->
        warn (UnsynchronizedAction act)
    _ -> return ()
  where
    syncs = Map.fromListWith (++) actions

    actions :: [(Loc Action, [(ComponentName, Name)])]
    actions =
        [ (actionName actionInfo, [(compName, name)])
        | (compName, moduleAlphabet) <- Map.assocs mas
        , (name, alph) <- Map.assocs moduleAlphabet
        , actionInfo <- Set.toList alph
        ]
