{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Module where


import Control.Lens
import Control.Monad.State

import           Data.Foldable
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import           Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.ComponentType
import Rbsc.Data.Name
import Rbsc.Data.System

import Rbsc.Syntax.Typed

import Rbsc.Translator.Alphabet
import Rbsc.Translator.Binding
import Rbsc.Translator.Command
import Rbsc.Translator.Internal
import Rbsc.Translator.Variable


trnsModules
    :: System
    -> BindingInfo
    -> Map ComponentName ModuleAlphabets
    -> Alphabets
    -> Map ComponentName [TNamedModuleBody Elem]
    -> Translator [Prism.Module]
trnsModules sys bi mas as bodiess = do
    compTys <- view componentTypes
    let oas = overrideActions as

    fmap concat . for (Map.assocs bodiess) $ \(name, bodies) ->
        case view (instances . at name) sys of
            Just typeName -> do
                let isRole = has (at typeName._Just._RoleType) compTys
                    cmas   = Map.findWithDefault Map.empty name mas
                for bodies $ \body -> do
                    let alph = Map.findWithDefault Set.empty
                                                   (view bodyName body)
                                                   cmas
                    trnsModule bi as alph oas isRole typeName name body
            Nothing -> error $ "trnsModules: undefined component " ++ show name


trnsModule
    :: BindingInfo
    -> Alphabets
    -> Alphabet
    -> OverrideActions
    -> Bool
    -> TypeName
    -> ComponentName
    -> TNamedModuleBody Elem
    -> Translator Prism.Module
trnsModule bi as alph oas isRole typeName compName (NamedModuleBody moduleName body) = do
    ident <- trnsQualified (QlMember (QlName (trnsComponentName compName)) moduleName)
    vars' <- trnsLocalVars typeName compName (bodyVars body)

    cmds' <- traverse
        (trnsCommand isRole typeName compName . getElem)
        (bodyCommands body)

    override <- genOverrideSelfLoops bi oas alph isRole compName
    nonblocking <- if isRole
        then genNonblockingSelfLoops bi as compName alph
        else return []

    return (Prism.Module ident vars' (concat [cmds', override, nonblocking]))


genOverrideSelfLoops
    :: MonadState TranslatorState m
    => BindingInfo
    -> OverrideActions
    -> Alphabet
    -> Bool
    -> ComponentName
    -> m [Prism.Command]
genOverrideSelfLoops bi oas alph isRole compName = traverse
    genCommand
    (filter (isModuleAction . snd)
            (toList (overrideActionsOfRoles bi oas compName)))
  where
    genCommand (roleName, act) = do
        act' <- trnsQualified (trnsAction act)
        let acts = [act', overrideActionIdent roleName]
            acts' =
                if isRole then acts ++ [notPlayedActionIdent compName] else acts
        return (selfLoops acts' Prism.ActionOpen)

    isModuleAction act = act `Set.member` alph'

    alph' = stripActionInfo alph


genNonblockingSelfLoops
    :: MonadState TranslatorState m
    => BindingInfo
    -> Alphabets
    -> RoleName
    -> Alphabet
    -> m [Prism.Command]
genNonblockingSelfLoops bi as roleName alph =
    traverse genCommand
        . filter ((&&) <$> isRequired <*> (not . isInternal))
        . toList
        $ stripActionInfo alph
  where
    genCommand act = do
        act' <- trnsQualified (trnsAction act)
        return (selfLoops [act', notPlayedActionIdent roleName] Prism.ActionOpen)

    isRequired = (`Set.member` required)
    isInternal = (`Set.member` internal)

    required = requiredActionsOfRole bi as roleName alph
    internal = internalActionsOfPlayers bi as roleName


selfLoops :: [Prism.Ident] -> Prism.ActionType -> Prism.Command
selfLoops acts actTy =
    Prism.Command (fmap Prism.Action acts) actTy (Prism.LitBool True) []
