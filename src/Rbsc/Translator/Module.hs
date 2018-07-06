{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Module where


import Control.Lens

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Semigroup
import           Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.Name
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Syntax.Typed

import Rbsc.Translator.Command
import Rbsc.Translator.Variable


trnsModules ::
       (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => System
    -> Map Name [TNamedModuleBody Elem]
    -> m [Prism.Module]
trnsModules sys bodiess =
    fmap concat . for (Map.assocs bodiess) $ \(name, bodies) ->
        case view (instances.at name) sys of
            Just typeName -> traverse (trnsModule typeName name) bodies
            Nothing -> error $ "trnsModules: undefined component " ++ show name


trnsModule ::
       (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => TypeName
    -> Name
    -> TNamedModuleBody Elem
    -> m Prism.Module
trnsModule typeName compName (NamedModuleBody moduleName body) = do
    vars' <- trnsLocalVars typeName compName (bodyVars body)
    cmds' <- traverse (trnsCommand typeName compName . getElem) (bodyCommands body)
    return (Prism.Module ident vars' cmds')
  where
    ident = compName <> "_" <> moduleName
