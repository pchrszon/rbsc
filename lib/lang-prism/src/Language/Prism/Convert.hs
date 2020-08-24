{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


-- | This module provides a conversion of multi-action PRISM to standard
-- PRISM using only single actions.
module Language.Prism.Convert
    ( convertToSingleActions

    , ConversionInfo
    , conversionInfo
    , numberOfActions
    , numberOfMultiActions
    ) where


import Control.Monad.State

import           Data.List  (foldl')
import           Data.Maybe
import           Data.Set   (Set, intersection, union)
import qualified Data.Set   as Set


import Language.Prism


-- | Convert a model with multi-actions to a model with single actions
convertToSingleActions
    :: Applicative f
    => ([Action] -> f Ident)
    -> ConversionInfo
    -> Model
    -> f Model
convertToSingleActions mkIdent ConversionInfo {..} model = mkModel
    <$> traverse (rewriteModule mkIdent ciMultiActionSet)
                 (modelModules ciModel)
    <*> traverse (rewriteRewardStruct mkIdent ciMultiActionSet)
                 (modelRewardStructs ciModel)
  where
    mkModel modules rewStructs = model
        { modelModules       = modules
        , modelRewardStructs = rewStructs
        }


-- | Compute the information necessary for converting a model with
-- multi-actions to a model with single actions.
conversionInfo :: Model -> ConversionInfo
conversionInfo model = ConversionInfo
    { ciMultiActionSet = mas
    , ciModel          = model'
    }
  where
    model' = insertTauActions model
    mas    = multiActionSet (modelModules model')


-- | Stores information needed for converting a model with multi-actions to
-- a model with single actions.
data ConversionInfo = ConversionInfo
    { ciMultiActionSet :: MultiActionSet
    , ciModel          :: Model
    }


-- | Returns the number of action names (i.e., the size of the action
-- alphabet) of the corresponding model.
numberOfActions :: ConversionInfo -> Int
numberOfActions ConversionInfo{..} = Set.size (alphabet ciMultiActionSet)


-- | Returns the number of composed multi-actions of the corresponding
-- model.
numberOfMultiActions :: ConversionInfo -> Int
numberOfMultiActions ConversionInfo{..} =
    Set.size (multiActions ciMultiActionSet)


-- | Insert 'Tau' actions for all open tau actions (@][@) in the model.
insertTauActions :: Model -> Model
insertTauActions model = flip evalState 0 $ do
    modules' <- traverse insertInModule (modelModules model)
    return model { modelModules = modules' }
  where
    insertInModule m = do
        cmds' <- traverse insertInCommand (modCommands m)
        return m { modCommands = cmds' }

    insertInCommand :: Command -> State Int Command
    insertInCommand (Command [] ActionOpen g upds) = do
        i <- get
        modify succ
        return (Command [Tau i] ActionOpen g upds)
    insertInCommand cmd = return cmd


rewriteModule
    :: Applicative f
    => ([Action] -> f Ident)
    -> MultiActionSet
    -> Module
    -> f Module
rewriteModule mkIdent mas m = fmap (mkModule . concat)
    (traverse (rewriteCommand mkIdent mas moduleAlphabet) (modCommands m))
  where
    mkModule cmds = m { modCommands = cmds }
    moduleAlphabet = getModuleAlphabet m


rewriteCommand
    :: Applicative f
    => ([Action] -> f Ident)
    -> MultiActionSet
    -> Set Action
    -> Command
    -> f [Command]
rewriteCommand mkIdent mas moduleAlphabet cmd =
    traverse (fmap mkCommand . mkSingleAction mkIdent) extensions
  where
    mkCommand act = cmd { cmdActions = [act], cmdActionType = ActionClosed }
    mas'       = Set.map actions (multiActions mas)
    extensions = actionExtensions mas'
                                  moduleAlphabet
                                  (cmdActions cmd)
                                  (cmdActionType cmd)


-- | Given a multi-action @a@, @actionExtensions@ returns the set of all
-- multi-actions in the model that synchronize with @a@.
actionExtensions
    :: Set (Set Action) -> Set Action -> [Action] -> ActionType -> [Set Action]
actionExtensions mas moduleAlphabet acts = \case
    ActionClosed
        | Set.null acts' -> [Set.empty]
        | otherwise      -> filter ((acts' ==) . removeTaus) (Set.toList mas)
    ActionOpen -> filter (canSync acts') (Set.toList mas)
  where
    acts' = Set.fromList acts
    canSync act actGlobal = act == actGlobal `intersection` moduleAlphabet


rewriteRewardStruct
    :: Applicative f
    => ([Action] -> f Ident)
    -> MultiActionSet
    -> RewardStruct
    -> f RewardStruct
rewriteRewardStruct mkIdent mas rs = fmap (mkRewardStruct . concat)
    (traverse (rewriteRewardStructItem mkIdent mas) (rsItems rs))
  where
    mkRewardStruct items = rs { rsItems = items }


rewriteRewardStructItem
    :: Applicative f
    => ([Action] -> f Ident)
    -> MultiActionSet
    -> RewardStructItem
    -> f [RewardStructItem]
rewriteRewardStructItem mkIdent mas ri = case riKind ri of
    TransitionReward acts ty ->
        traverse (fmap mkTransitionRewardItem . mkSingleAction mkIdent)
        (matchingActions mas' acts ty)
    StateReward -> pure [ri]
  where
    mkTransitionRewardItem act =
        ri { riKind = TransitionReward [act] ActionClosed }
    mas' = Set.map actions (multiActions mas)


matchingActions :: Set (Set Action) -> [Action] -> ActionType -> [Set Action]
matchingActions mas acts = \case
    ActionClosed
        | Set.null acts' -> [Set.empty]
        | otherwise      -> filter ((acts' ==) . removeTaus) (Set.toList mas)
    ActionOpen -> filter (matches acts') (Set.toList mas)
  where
    acts' = Set.fromList acts
    matches act actGlobal = act `Set.isSubsetOf` actGlobal


mkSingleAction
    :: Applicative f => ([Action] -> f Ident) -> Set Action -> f Action
mkSingleAction mkIdent = fmap Action . mkIdent . Set.toList


data MultiAction = MultiAction
    { actions    :: !(Set Action)
    , actionType :: !ActionType
    } deriving (Eq, Ord, Show)


data MultiActionSet = MultiActionSet
    { multiActions :: !(Set MultiAction)
    , alphabet     :: !(Set Action)
    } deriving (Show)

instance Semigroup MultiActionSet where
    (<>) = composeSets

instance Monoid MultiActionSet where
    mempty = MultiActionSet mempty mempty


multiActionSet :: [Module] -> MultiActionSet
multiActionSet = foldl' (<>) mempty . fmap fromModule
  where
    fromModule m = MultiActionSet
        { multiActions = Set.fromList (fmap multiAction (modCommands m))
        , alphabet     = getModuleAlphabet m
        }
    multiAction (Command act actTy _ _) = MultiAction (Set.fromList act) actTy


getModuleAlphabet :: Module -> Set Action
getModuleAlphabet = Set.fromList . concatMap cmdActions . modCommands


composeSets :: MultiActionSet -> MultiActionSet -> MultiActionSet
composeSets (MultiActionSet mas1 alphabet1) (MultiActionSet mas2 alphabet2) =
    MultiActionSet
        { multiActions = Set.unions [interleaved1, interleaved2, synchronized]
        , alphabet     = alphabet1 `union` alphabet2
        }
  where
    synch = alphabet1 `intersection` alphabet2
    interleaved1 = interleaved synch mas1
    interleaved2 = interleaved synch mas2

    synchronized = Set.fromList . catMaybes $
        [ compose synch ma1 ma2
        | ma1 <- Set.toList mas1
        , ma2 <- Set.toList mas2
        ]


interleaved :: Set Action -> Set MultiAction -> Set MultiAction
interleaved synch = Set.filter (Set.null . Set.intersection synch . actions)


compose :: Set Action -> MultiAction -> MultiAction -> Maybe MultiAction
compose synch (MultiAction act1 ActionClosed) (MultiAction act2 ActionClosed)
    | removeTaus act1 == removeTaus act2 &&
          not (Set.null (act1 `intersection` synch)) =
        Just (MultiAction act1 ActionClosed)
    | otherwise = Nothing
compose synch (MultiAction act1 ActionOpen) (MultiAction act2 ActionClosed) =
    composedOpenClosed synch act1 act2
compose synch (MultiAction act1 ActionClosed) (MultiAction act2 ActionOpen) =
    composedOpenClosed synch act2 act1
compose synch (MultiAction act1 ActionOpen) (MultiAction act2 ActionOpen)
    | act1 `intersection` synch == act2 `intersection` synch =
        Just (MultiAction (act1 `union` act2) ActionOpen)
    | otherwise = Nothing


composedOpenClosed
    :: Set Action -> Set Action -> Set Action -> Maybe MultiAction
composedOpenClosed synch act1 act2
    | not (Set.null act2) && removeTaus act1 == act2 `intersection` synch =
        Just (MultiAction (act1 `union` act2) ActionClosed)
    | otherwise = Nothing


removeTaus :: Set Action -> Set Action
removeTaus = Set.filter (not . isTauAction)
  where
    isTauAction = \case
        Tau _ -> True
        _     -> False
