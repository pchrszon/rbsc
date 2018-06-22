{-# LANGUAGE LambdaCase #-}


-- | Internal representation of actions.
module Rbsc.Data.Action
    ( Action(..)
    , ActionKind(..)
    ) where


import Data.Text.Prettyprint.Doc


import Rbsc.Data.Name

import Rbsc.Report.Region


-- | An action used for synchronization.
data Action
    = Action !Name
    | LocalAction !Name !Name
    | IndexedAction Action !Int
    deriving (Eq, Ord, Show)

instance Pretty Action where
    pretty = \case
        Action name -> pretty name
        LocalAction name member -> pretty name <> dot <> pretty member
        IndexedAction act idx -> pretty act <> brackets (pretty idx)


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
