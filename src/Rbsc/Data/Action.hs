{-# LANGUAGE LambdaCase #-}


-- | Internal representation of actions.
module Rbsc.Data.Action
    ( Action(..)
    , ActionInfo(..)
    , ActionKind(..)
    , ActionIntent(..)

    , isOverrideAction
    , isInternalAction
    ) where


import Data.Text.Prettyprint.Doc


import Rbsc.Data.Name

import Rbsc.Report.Region


-- | An action used for synchronization.
data Action
    = Action !Name
    | LocalAction !ComponentName !Name
    | IndexedAction Action !Int
    deriving (Eq, Ord, Show)

instance Pretty Action where
    pretty = \case
        Action name -> pretty name
        LocalAction name member -> pretty name <> dot <> pretty member
        IndexedAction act idx -> pretty act <> brackets (pretty idx)


-- | An 'Action' boundled with its 'ActionKind' and its 'ActionIntent'.
data ActionInfo = ActionInfo
    { actionName   :: !(Loc Action)
    , actionKind   :: !ActionKind
    , actionIntent :: !ActionIntent
    } deriving (Eq, Ord, Show)


-- | An action can be either normal or overriding.
data ActionKind
    = NormalAction
    | OverrideAction !Region
    deriving (Show)

instance Eq ActionKind where
    NormalAction     == NormalAction     = True
    OverrideAction _ == OverrideAction _ = True
    _                == _                = False

instance Ord ActionKind where
    compare NormalAction = \case
        NormalAction     -> EQ
        OverrideAction _ -> LT
    compare (OverrideAction _) = \case
        NormalAction     -> GT
        OverrideAction _ -> EQ


-- | If an action is intended to be internal, then it should only be
-- executed if a bound role synchronizes with it. Otherwise, it should
-- block. However, if a component has no bound roles, then internal actions
-- behave exactly the same as external actions.
data ActionIntent
    = ExternalAction
    | InternalAction
    deriving (Eq, Ord, Show)


-- | Returns 'True' if the given action is an 'OverrideAction'.
isOverrideAction :: ActionInfo -> Bool
isOverrideAction (ActionInfo _ k _) = case k of
    OverrideAction _ -> True
    NormalAction     -> False


-- | Returns 'True' if the given action is an 'InternalAction'.
isInternalAction :: ActionInfo -> Bool
isInternalAction = (== InternalAction) . actionIntent
