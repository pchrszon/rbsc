module Rbsc.Translator
    ( translateModel
    ) where


import Control.Monad.Reader

import qualified Language.Prism as Prism


import Rbsc.Data.Info
import Rbsc.Data.System

import Rbsc.Report.Result

import Rbsc.Syntax.Typed

import Rbsc.Translator.Instantiation
import Rbsc.Translator.Internal
import Rbsc.Translator.Module
import Rbsc.Translator.Variable


translateModel :: Model -> System -> Info -> Result Prism.Model
translateModel model sys info = do
    modules <- runReaderT (instantiateComponents model sys) info
    (globals', modules') <- runTranslator info $
        (,) <$> trnsGlobalVars (modelGlobals model) <*> trnsModules sys modules

    return Prism.Model
        { Prism.modelType       = Prism.MDP
        , Prism.modelFormulas   = []
        , Prism.modelLabels     = []
        , Prism.modelConstants  = []
        , Prism.modelGlobalVars = fmap Prism.GlobalVar globals'
        , Prism.modelModules    = modules'
        , Prism.modelInitStates = Nothing
        }
