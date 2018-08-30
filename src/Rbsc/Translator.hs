{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Translator
    ( translateModels
    , translateModel
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.Foldable
import qualified Data.Map.Strict  as Map
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
import Rbsc.Translator.Instantiation
import Rbsc.Translator.Internal
import Rbsc.Translator.Module
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
    modules      <- runReaderT (instantiateComponents model sys) info
    coordinators <- runReaderT
        (traverse instantiateCoordinator (modelCoordinators model))
        info

    mas <- moduleAlphabets modules
    let as = componentAlphabets mas
    bi <- generateBindingInfo sys as

    runTranslator info $ do
        globals' <- trnsGlobalVars (modelGlobals model)
        modules' <- trnsModules sys bi mas as modules
        coordinators' <-
            trnsCoordinators (view componentTypes info) sys bi as coordinators
        desync <- genDesyncModule as

        return Prism.Model
            { Prism.modelType       = Prism.MDP
            , Prism.modelFormulas   = []
            , Prism.modelLabels     = []
            , Prism.modelConstants  = []
            , Prism.modelGlobalVars = fmap Prism.GlobalVar globals'
            , Prism.modelModules    = concat [coordinators', [desync], modules']
            , Prism.modelInitStates = Nothing
            }


genDesyncModule :: Alphabets -> Translator Prism.Module
genDesyncModule as = do
    ident <- trnsQualified (QlName "Desync")
    cmds <- traverse genCommand acts
    return (Prism.Module ident [] cmds)
  where
    acts = toList (Set.map (unLoc . fst) (Set.unions (Map.elems as)))

    genCommand :: Action -> Translator Prism.Command
    genCommand act = do
        act' <- trnsQualified (trnsAction act)
        return (Prism.Command [act'] Prism.ActionOpen (Prism.LitBool True) [])
