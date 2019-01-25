{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Module
    ( trnsModules
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Foldable
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (maybeToList)
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
    -> Map ComponentName [TModuleInstance Elem]
    -> Translator [Prism.Module]
trnsModules sys bi mas as instss = do
    compTys <- view componentTypes
    let oas = overrideActions as

    fmap concat . for (Map.assocs instss) $ \(name, insts) ->
        case view (instances . at name) sys of
            Just typeName -> do
                let isRole = has (at typeName._Just._RoleType) compTys
                    cmas   = Map.findWithDefault Map.empty name mas
                for insts $ \inst -> do
                    let alph = Map.findWithDefault Set.empty
                                                   (view miName inst)
                                                   cmas
                    trnsModule bi as alph oas isRole typeName name inst
            Nothing -> error $ "trnsModules: undefined component " ++ show name


trnsModule
    :: BindingInfo
    -> Alphabets
    -> Alphabet
    -> OverrideActions
    -> Bool
    -> TypeName
    -> ComponentName
    -> TModuleInstance Elem
    -> Translator Prism.Module
trnsModule bi as alph oas isRole typeName compName (ModuleInstance moduleName _ body) = do
    ident <- trnsQualified
        (QlMember (QlName (trnsComponentName compName)) moduleName)

    vars'       <- trnsLocalVars typeName compName (bodyVars body)
    activityVar <- maybeToList <$> genRoleActivityVar compName
    let vars'' = activityVar ++ vars'

    cmds' <- traverse
        (trnsCommand isRole typeName compName . getElem)
        (bodyCommands body)
    resetCmd <- maybeToList <$> genResetCommand compName

    override <- genOverrideSelfLoops bi oas alph isRole compName
    nonblocking <- if isRole
        then genNonblockingSelfLoops bi as compName alph
        else return []

    return (Prism.Module ident vars''
        (concat [cmds', override, nonblocking, resetCmd]))


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
        return (selfLoops acts' [])

    isModuleAction act = act `Set.member` alph'

    alph' = stripActionInfo alph


genNonblockingSelfLoops
    :: BindingInfo
    -> Alphabets
    -> RoleName
    -> Alphabet
    -> Translator [Prism.Command]
genNonblockingSelfLoops bi as roleName alph =
    traverse genCommand
        . filter ((&&) <$> isRequired <*> (not . isInternal))
        . toList
        $ stripActionInfo alph
  where
    genCommand act = do
        act' <- trnsQualified (trnsAction act)

        obsRoles <- view observedRoles
        let upds =
                [ activityVarUpdate (trnsComponentName roleName)
                | roleName `elem` obsRoles
                ]

        return (selfLoops [act', notPlayedActionIdent roleName] upds)

    isRequired = (`Set.member` required)
    isInternal = (`Set.member` internal)

    required = requiredActionsOfRole bi as roleName alph
    internal = internalActionsOfPlayers bi as roleName

    activityVarUpdate name = Prism.Update Nothing [(name, Prism.LitInt (-1))]


selfLoops :: [Prism.Ident] -> [Prism.Update] -> Prism.Command
selfLoops acts upds = Prism.Command
    { Prism.cmdActions    = fmap Prism.Action acts
    , Prism.cmdActionType = Prism.ActionOpen
    , Prism.cmdGuard      = Prism.LitBool True
    , Prism.cmdUpdates    = upds
    }


genRoleActivityVar :: ComponentName -> Translator (Maybe Prism.Declaration)
genRoleActivityVar compName = do
    obsRoles <- view observedRoles
    return $ if compName `elem` obsRoles
        then Just (genDecl (trnsComponentName compName))
        else Nothing
  where
    genDecl name = Prism.Declaration
        name
        (Prism.DeclTypeInt (Prism.LitInt (-1)) (Prism.LitInt 1))
        (Just (Prism.LitInt 0))


genResetCommand :: ComponentName -> Translator (Maybe Prism.Command)
genResetCommand compName = do
    obsRoles <- view observedRoles
    return $ if compName `elem` obsRoles
        then Just (resetCommand (trnsComponentName compName))
        else Nothing
  where
    resetCommand name = Prism.Command
        { Prism.cmdActions    = [Prism.Action "reset"]
        , Prism.cmdActionType = Prism.ActionClosed
        , Prism.cmdGuard = Prism.UnaryOp Prism.Not (Prism.Ident stepGuardName)
        , Prism.cmdUpdates = [Prism.Update Nothing [(name, Prism.LitInt 0)]]
        }
