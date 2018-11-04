{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


-- | Abstract syntax of reward structures.
module Rbsc.Syntax.RewardStruct
    ( RewardStruct(..)
    , RewardStructItem(..)
    , RewardKind(..)
    ) where


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Coordinator
import Rbsc.Syntax.Impl
import Rbsc.Syntax.Typed.Expr


-- | A reward structure.
data RewardStruct elem ty expr = RewardStruct
    { rsName  :: Maybe (Loc Name)
    , rsItems :: [elem ty expr (RewardStructItem expr)]
    }

deriving instance (Show ty, Show expr) => Show (RewardStruct ElemMulti ty expr)
deriving instance (Show ty, Show expr) => Show (RewardStruct Elem ty expr)


-- | An item in a reward structure.
data RewardStructItem expr = RewardStructItem
    { riKind   :: RewardKind expr
    , riGuard  :: expr
    , riReward :: expr
    } deriving (Show)

instance HasExprs expr => HasExprs (RewardStructItem expr) where
    exprs f RewardStructItem{..} = RewardStructItem
        <$> exprs f riKind
        <*> exprs f riGuard
        <*> exprs f riReward


-- | A reward item can either define a transition reward or a state reward.
data RewardKind expr
    = TransitionReward (Maybe expr) (Maybe (PlayingConstraint expr))
    | StateReward
    deriving (Show)

instance HasExprs expr => HasExprs (RewardKind expr) where
    exprs f = \case
        TransitionReward mAct mConstr -> TransitionReward
            <$> traverse (exprs f) mAct
            <*> traverse (exprs f) mConstr
        StateReward -> pure StateReward
