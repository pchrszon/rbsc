{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Rbsc.Translator
    ( translateModels
    , translateModel
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.Foldable
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import qualified Data.Set         as Set
import           Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Config

import Rbsc.Data.Action
import Rbsc.Data.ComponentType
import Rbsc.Data.Info
import Rbsc.Data.System

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


translateModels :: RecursionDepth -> U.Model -> Result [(System, Prism.Model)]
translateModels depth model = do
    (typedModel, sysInfos) <- flip runReaderT depth $ do
        (typedModel, mi) <- typeCheck model
        sysInfos <- generateInstances typedModel mi
        return (typedModel, sysInfos)

    for sysInfos $ \(sys, mi) -> do
        model' <- translateModel typedModel sys (Info mi depth)
        return (sys, model')


translateModel :: Model -> System -> Info -> Result Prism.Model
translateModel model sys info = do
    (modules, coordinators, rewardStructs) <- flip runReaderT info $ (,,)
        <$> instantiateComponents model sys
        <*> traverse instantiateCoordinator (modelCoordinators model)
        <*> traverse instantiateRewardStruct (modelRewardStructs model)

    mas <- moduleAlphabets modules
    let as = componentAlphabets mas
    bi <- generateBindingInfo sys as

    checkActionIndices mas

    runTranslator info $ do
        desync   <- maybeToList <$> genDesyncModule as
        globals' <- trnsGlobalVars (modelGlobals model)
        labels'  <- traverse trnsLabel (modelLabels model)
        modules' <- trnsModules sys bi mas as modules
        coordinators' <-
            trnsCoordinators (view componentTypes info) sys bi as coordinators
        rewardStructs' <-
            trnsRewardStructs rewardStructs

        return Prism.Model
            { Prism.modelType          = Prism.MDP
            , Prism.modelFormulas      = []
            , Prism.modelLabels        = labels'
            , Prism.modelConstants     = []
            , Prism.modelGlobalVars    = fmap Prism.GlobalVar globals'
            , Prism.modelModules       =
                concat [coordinators', desync, modules']
            , Prism.modelInitStates    = Nothing
            , Prism.modelRewardStructs = rewardStructs'
            }


trnsLabel :: TLabel -> Translator Prism.Label
trnsLabel Label{..} = do
    e' <- reduceLSomeExpr labelExpr
    Prism.Label (unLoc labelName) <$> trnsLSomeExpr Nothing e'


genDesyncModule :: Alphabets -> Translator (Maybe Prism.Module)
genDesyncModule as = do
    ident <- trnsQualified (QlName "Desync")
    cmds <- traverse genCommand acts
    return $ if null cmds
        then Nothing
        else Just (Prism.Module ident [] cmds)
  where
    acts = toList (stripActionInfo (Set.unions (Map.elems as)))

    genCommand :: Action -> Translator Prism.Command
    genCommand act = do
        act' <- trnsQualified (trnsAction act)
        return (Prism.Command
            [Prism.Action act']
            Prism.ActionOpen
            (Prism.LitBool True) [])
