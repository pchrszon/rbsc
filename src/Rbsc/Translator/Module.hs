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
    -> Alphabets
    -> Map Name [TNamedModuleBody Elem]
    -> Translator [Prism.Module]
trnsModules sys as bodiess = do
    compTys <- view componentTypes

    bi <- lift (lift (generateBindingInfo sys as))
    let oas = overrideActions as

    fmap concat . for (Map.assocs bodiess) $ \(name, bodies) ->
        case view (instances.at name) sys of
            Just typeName -> do
                let isRole = has (at typeName._Just._RoleType) compTys
                    alph   = Map.findWithDefault Set.empty name as
                traverse (trnsModule bi as alph oas isRole typeName name) bodies
            Nothing -> error $ "trnsModules: undefined component " ++ show name


trnsModule
    :: BindingInfo
    -> Alphabets
    -> Alphabet
    -> OverrideActions
    -> Bool
    -> TypeName
    -> Name
    -> TNamedModuleBody Elem
    -> Translator Prism.Module
trnsModule bi as alph oas isRole typeName compName (NamedModuleBody moduleName body) = do
    ident <- trnsQualified (QlMember (QlName compName) moduleName)
    vars' <- trnsLocalVars typeName compName (bodyVars body)

    cmds' <- traverse
        (trnsCommand isRole typeName compName . getElem)
        (bodyCommands body)

    override <- genOverrideSelfLoops bi oas compName
    nonblocking <- if isRole
        then genNonblockingSelfLoops bi as compName alph
        else return []

    return (Prism.Module ident vars' (concat [cmds', override, nonblocking]))


-- TODO: only generate self-loops for actions in own module alphabet
genOverrideSelfLoops
    :: MonadState TranslatorState m
    => BindingInfo
    -> OverrideActions
    -> Name
    -> m [Prism.Command]
genOverrideSelfLoops bi oas =
    traverse genCommand . toList . overrideActionsOfRoles bi oas
  where
    genCommand (roleName, act) = do
        act' <- trnsQualified (trnsAction act)
        let acts = [act', overrideActionIdent roleName]
        return (selfLoops acts Prism.ActionOpen)


-- TODO: only generate self-loops for actions in own module alphabet
genNonblockingSelfLoops
    :: MonadState TranslatorState m
    => BindingInfo
    -> Alphabets
    -> RoleName
    -> Alphabet
    -> m [Prism.Command]
genNonblockingSelfLoops bi as roleName alph =
    traverse genCommand (filter isRequired (toList (stripLocAndKind alph)))
  where
    genCommand act = do
        act' <- trnsQualified (trnsAction act)
        return (selfLoops [act'] Prism.ActionOpen)

    isRequired = (`Set.member` required)

    required = requiredActionsOfRole bi as roleName alph


selfLoops :: [Prism.Ident] -> Prism.ActionType -> Prism.Command
selfLoops acts actTy = Prism.Command acts actTy (Prism.LitBool True) []
