{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Rbsc.Translator.RewardStruct
    ( trnsRewardStructs
    ) where


import qualified Data.Map.Strict as Map
import           Data.Maybe      (maybeToList)

import qualified Language.Prism as Prism


import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Command              (trnsActionExpr)
import Rbsc.Translator.Coordinator.Internal
import Rbsc.Translator.Expr
import Rbsc.Translator.Internal


trnsRewardStructs :: [TRewardStruct Elem] -> Translator [Prism.RewardStruct]
trnsRewardStructs = traverse trnsRewardStruct


trnsRewardStruct :: TRewardStruct Elem -> Translator Prism.RewardStruct
trnsRewardStruct RewardStruct{..} = do
    let name' = fmap unLoc rsName
    items' <- concat <$> traverse (trnsRewardStructItem . getElem) rsItems
    return (Prism.RewardStruct name' items')


trnsRewardStructItem :: TRewardStructItem -> Translator [Prism.RewardStructItem]
trnsRewardStructItem RewardStructItem{..} = do
    rewKinds <- trnsRewardKind riKind
    grd' <- trnsLSomeExpr Nothing riGuard
    rew' <- trnsLSomeExpr Nothing riReward
    return (fmap (mkItem grd' rew') rewKinds)
  where
    mkItem grd rew k = Prism.RewardStructItem k grd rew


trnsRewardKind :: TRewardKind -> Translator [Prism.RewardKind]
trnsRewardKind = \case
    TransitionReward mAct mConstr -> do
        act' <- traverse trnsActionExpr (maybeToList mAct)
        constrs' <- case mConstr of
            Just constr -> trnsPlayingConstraint constr
            Nothing     -> return []
        return (fmap (mkTransitionReward act') constrs')
    StateReward -> return [Prism.StateReward]
  where
    mkTransitionReward act' constr' = Prism.TransitionReward
        (fmap Prism.Action (act' ++ constr'))
        Prism.ActionOpen


trnsPlayingConstraint :: TPlayingConstraint -> Translator [[Prism.Ident]]
trnsPlayingConstraint constr = case constr of
    PlayingConstraint (Loc (SomeExpr c TyBool) rgn) _ -> do
        roles <- rolesInConstraint constr
        let vals = allValuatations roles
        sats <- satisfyingValuations vals (Loc c rgn)

        return (fmap (fmap toAction . Map.assocs) sats)
    _ -> error "trnsPlayingConstraint: type error"
  where
    toAction (roleName, b)
        | b         = playedActionIdent roleName
        | otherwise = notPlayedActionIdent roleName
