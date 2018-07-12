{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Translator
    ( translateModel
    ) where


import Control.Monad.Reader

import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Language.Prism as Prism


import Rbsc.Data.Action
import Rbsc.Data.Info
import Rbsc.Data.System

import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Typed

import Rbsc.Translator.Alphabet
import Rbsc.Translator.Instantiation
import Rbsc.Translator.Internal
import Rbsc.Translator.Module
import Rbsc.Translator.Variable


translateModel :: Model -> System -> Info -> Result Prism.Model
translateModel model sys info = do
    modules <- runReaderT (instantiateComponents model sys) info
    as <- alphabets modules

    runTranslator info $ do
        globals' <- trnsGlobalVars (modelGlobals model)
        modules' <- trnsModules sys as modules
        desync   <- genDesyncModule as

        return Prism.Model
            { Prism.modelType       = Prism.MDP
            , Prism.modelFormulas   = []
            , Prism.modelLabels     = []
            , Prism.modelConstants  = []
            , Prism.modelGlobalVars = fmap Prism.GlobalVar globals'
            , Prism.modelModules    = modules' ++ [desync]
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
