{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}


module Rbsc.Translator
    ( translateModels
    , translateModel
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict    as Map
import           Data.Maybe
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Config

import Rbsc.Data.Action
import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.Field
import Rbsc.Data.ModelInfo
import Rbsc.Data.System

import Rbsc.Eval

import Rbsc.Instantiation

import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed
import qualified Rbsc.Syntax.Untyped as U

import Rbsc.Translator.Alphabet
import Rbsc.Translator.Binding
import Rbsc.Translator.Coordinator
import Rbsc.Translator.Expr
import Rbsc.Translator.Instantiation
import Rbsc.Translator.Internal
import Rbsc.Translator.Module
import Rbsc.Translator.RewardStruct
import Rbsc.Translator.Variable

import Rbsc.TypeChecker


translateModels
    :: RecursionDepth
    -> U.Model
    -> Result (NonEmpty (System, ModelInfo, Prism.Model))
translateModels depth model = do
    (typedModel, sysInfos) <- flip runReaderT depth $ do
        (typedModel, mi) <- typeCheck model
        sysInfos <- generateInstances typedModel mi
        return (typedModel, sysInfos)

    for sysInfos $ \(sys, mi) -> do
        model' <- translateModel typedModel sys (mi :&: depth)
        return (sys, mi, model')


translateModel
    :: Model -> System -> (ModelInfo :&: RecursionDepth) -> Result Prism.Model
translateModel model sys info = do
    (modules, coordinators, rewardStructs, obsRoles) <-
        flip runReaderT info $ (,,,)
            <$> instantiateComponents model sys
            <*> traverse instantiateCoordinator (modelCoordinators model)
            <*> traverse instantiateRewardStruct (modelRewardStructs model)
            <*> getObservedRoles (modelObserve model)

    mas <- moduleAlphabets modules
    cas <- Set.unions <$> traverse coordinatorActions coordinators
    let as = componentAlphabets mas
        nosyncActs = stripActionInfo (Set.unions (Map.elems as)) `Set.union` cas
    bi <- generateBindingInfo sys as

    checkActionIndices mas
    checkSynchronizations mas
    checkOverrides bi as

    let rgs = rolePlayingGuards sys modules info

    runTranslator (rgs :&: obsRoles :&: info) $ do
        nosync   <- maybeToList <$> genNosyncModule nosyncActs
        step     <- genStepFormula
        globals' <- trnsGlobalVars (modelGlobals model)
        labels'  <- traverse trnsLabel (modelLabels model)
        modules' <- trnsModules sys bi mas as modules
        coordinators' <-
            trnsCoordinators (view componentTypes info) sys bi as coordinators
        rewardStructs' <-
            trnsRewardStructs rewardStructs

        return Prism.Model
            { Prism.modelType          = Prism.MDP
            , Prism.modelFormulas      = step
            , Prism.modelLabels        = labels'
            , Prism.modelConstants     = []
            , Prism.modelGlobalVars    = fmap Prism.GlobalVar globals'
            , Prism.modelModules       =
                concat [nosync, coordinators', modules']
            , Prism.modelInitStates    = Nothing
            , Prism.modelRewardStructs = rewardStructs'
            }


trnsLabel :: TLabel -> Translator Prism.Label
trnsLabel Label{..} = do
    e' <- reduceLSomeExpr labelExpr
    Prism.Label (unLoc labelName) <$> trnsLSomeExpr Nothing e'


getObservedRoles :: MonadEval r m => [Loc (Expr Component)] -> m [RoleName]
getObservedRoles =
    fmap (fmap (view compName)) . traverse eval


genNosyncModule :: Set Action -> Translator (Maybe Prism.Module)
genNosyncModule acts
    | length acts <= 1 = return Nothing
    | otherwise = do
        ident <- trnsQualified (QlName "Nosync")
        cmds <- traverse genCommand (toList acts)
        return (Just (Prism.Module ident [] cmds))
  where
    genCommand act = do
        act' <- trnsQualified (trnsAction act)
        return (Prism.Command
            [Prism.Action act']
            Prism.ActionOpen
            (Prism.LitBool True) [])


genStepFormula :: Translator [Prism.Formula]
genStepFormula = view observedRoles >>= \case
    []       -> return []
    obsRoles -> return [Prism.Formula stepGuardName (stepExpr obsRoles)]
  where
    stepExpr =
        foldr1 (binOp Prism.And) .
        fmap (equalsZero . Prism.Ident . trnsComponentName)

    equalsZero e = binOp Prism.Eq e (Prism.LitInt 0)
    binOp = flip Prism.BinaryOp
