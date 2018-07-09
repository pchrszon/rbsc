{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Module where


import Control.Lens
import Control.Monad.Trans

import           Data.Foldable
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Semigroup
import qualified Data.Set         as Set
import           Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.ComponentType
import Rbsc.Data.Name
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Typed

import Rbsc.Translator.Alphabet
import Rbsc.Translator.Binding
import Rbsc.Translator.Command
import Rbsc.Translator.Internal
import Rbsc.Translator.Variable


trnsModules ::
       ( MonadEval r (t Result)
       , HasSymbolTable r
       , HasRangeTable r
       , HasComponentTypes r
       , MonadTrans t
       )
    => System
    -> Map Name [TNamedModuleBody Elem]
    -> t Result [Prism.Module]
trnsModules sys bodiess = do
    compTys <- view componentTypes

    as <- alphabets bodiess
    binds <- lift (generateBindings sys as)
    let oas = overrideActions as

    fmap concat . for (Map.assocs bodiess) $ \(name, bodies) ->
        case view (instances.at name) sys of
            Just typeName -> do
                let isRole = has (at typeName._Just._RoleType) compTys
                    alph   = Map.findWithDefault Set.empty name as
                traverse (trnsModule binds alph oas isRole typeName name) bodies
            Nothing -> error $ "trnsModules: undefined component " ++ show name


trnsModule ::
       (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => Bindings
    -> Alphabet
    -> OverrideActions
    -> Bool
    -> TypeName
    -> Name
    -> TNamedModuleBody Elem
    -> m Prism.Module
trnsModule binds alph oas isRole typeName compName (NamedModuleBody moduleName body) = do
    vars' <- trnsLocalVars typeName compName (bodyVars body)

    cmds' <- traverse
        (trnsCommand isRole typeName compName . getElem)
        (bodyCommands body)

    override <- genOverrideSelfLoops binds oas compName
    nonblocking <- if isRole
        then genNonblockingSelfLoops alph
        else return []

    return (Prism.Module ident vars' (concat [cmds', override, nonblocking]))
  where
    ident = compName <> "_" <> moduleName


genOverrideSelfLoops ::
       Monad m => Bindings -> OverrideActions -> Name -> m [Prism.Command]
genOverrideSelfLoops binds oas =
    traverse genCommand . toList . overrideActionsOfRoles binds oas
  where
    genCommand (roleName, act) = do
        act' <- trnsQualified (trnsAction act)
        let acts = [act', overrideActionIdent roleName]
        return (selfLoops acts Prism.ActionOpen)


genNonblockingSelfLoops :: Monad m => Alphabet -> m [Prism.Command]
genNonblockingSelfLoops = traverse genCommand . toList . Set.map (unLoc . fst)
  where
    genCommand act = do
        act' <- trnsQualified (trnsAction act)
        return (selfLoops [act'] Prism.ActionOpen)


selfLoops :: [Prism.Ident] -> Prism.ActionType -> Prism.Command
selfLoops acts actTy = Prism.Command acts actTy (Prism.LitBool True) []
