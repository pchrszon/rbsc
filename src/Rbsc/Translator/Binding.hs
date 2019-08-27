{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}


-- | This module provides functionality to generate role bindings and
-- check for incompatibilities between roles.
module Rbsc.Translator.Binding
    ( BindingInfo
    , rolesOfComponent
    , playersOfRole
    , generateBindingInfo

    , requiredActionsOfRole
    , internalActionsOfPlayers
    , overrideActionsOfRoles

    , checkOverrides
    ) where


import Control.Lens
import Control.Monad

import           Data.Foldable
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Traversable


import Rbsc.Data.Action
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Translator.Alphabet

import Rbsc.Util (renderPretty)


data BindingInfo = BindingInfo
    { _rolesOfComponent :: Map ComponentName (Set RoleName)
    , _playersOfRole    :: Map RoleName (Set ComponentName)
    }


rolesOfComponent :: BindingInfo -> ComponentName -> Set RoleName
rolesOfComponent bi compName =
    Map.findWithDefault Set.empty compName (_rolesOfComponent bi)


playersOfRole :: BindingInfo -> RoleName -> Set ComponentName
playersOfRole bi roleName =
    Map.findWithDefault Set.empty roleName (_playersOfRole bi)


-- | Generate the 'BindingInfo' for a given 'System'. If there are
-- incompatible roles bound to the same component, 'IncompatibleRoles'
-- errors are thrown.
generateBindingInfo :: System -> Alphabets -> Result BindingInfo
generateBindingInfo sys as = do
    binds <- globalBindings sys as
    return BindingInfo
        { _rolesOfComponent = invert binds
        , _playersOfRole    = fromBindings binds
        }
  where
    fromBindings = Map.fromList . fmap fromBinding
    fromBinding (Binding roleName ps) = (roleName, Set.fromList ps)

    invert = Map.unionsWith Set.union . fmap (\(Binding role ps) ->
        Map.unions (fmap (`Map.singleton` Set.singleton role) ps))


-- | The required actions of a role. Required actions are actions where the
-- role and its player synchronize or actions that are overridden by the
-- role.
requiredActionsOfRole
    :: BindingInfo -> Alphabets -> RoleName -> Alphabet -> Set Action
requiredActionsOfRole bi as roleName alph =
    stripActionInfo alph `Set.intersection` playerAlphabets
  where
    playerAlphabets = stripActionInfo (alphabetOfPlayers bi as roleName)


-- | The internal actions of all players of a given role.
internalActionsOfPlayers :: BindingInfo -> Alphabets -> RoleName -> Set Action
internalActionsOfPlayers bi as roleName = stripActionInfo
    (Set.filter isInternalAction (alphabetOfPlayers bi as roleName))


-- | Get the combined 'Alphabet' of all players of a given role.
alphabetOfPlayers :: BindingInfo -> Alphabets -> RoleName -> Alphabet
alphabetOfPlayers bi as roleName =
    Set.unions (fmap getAlphabet (toList (playersOfRole bi roleName)))
  where
    getAlphabet compName = Map.findWithDefault Set.empty compName as


-- | Get the set of actions that are overridden in the given component.
overrideActionsOfRoles ::
       BindingInfo -> OverrideActions -> ComponentName -> Set (RoleName, Action)
overrideActionsOfRoles bi oas compName =
    Set.unions . flip fmap (toList roles) $ \role ->
        Set.map ((,) role) (Map.findWithDefault Set.empty role oas)
  where
    roles = rolesOfComponent bi compName


-- | Check if all actions marked override actually exist within the player
-- components.
checkOverrides :: BindingInfo -> Alphabets -> Result ()
checkOverrides bi as =
    traverse_ checkOverridesOfRole (Map.keys (_playersOfRole bi))
  where
    checkOverridesOfRole roleName = do
        let alph   = Map.findWithDefault Set.empty roleName as
            ovActs = Set.filter isOverrideAction alph
            rolePlayers = toList
                (Map.findWithDefault Set.empty roleName (_playersOfRole bi))
        for_ rolePlayers $ \compName -> do
            let playerActs =
                    stripActionInfo (Map.findWithDefault Set.empty compName as)
            for_ ovActs $ \act ->
                unless (unLoc (actionName act) `Set.member` playerActs) $
                    warn (MissingOverriddenAction (actionName act) compName)


data Binding = Binding
    { _bindRole    :: !RoleName
    , _bindPlayers :: [ComponentName]
    } deriving (Show)


globalBindings :: System -> Alphabets -> Result [Binding]
globalBindings sys as =
    fmap concat . for (toList (coreComponents sys)) $ \core -> do
        checkCompatibilities sys plays as core
        return (localBindings sys plays core)
  where
    plays = buildPlaysRelation sys


localBindings :: System -> PlaysRelation -> ComponentName -> [Binding]
localBindings sys plays core =
    concatMap (go (core : containedPlayers)) (getRoles core)
  where
    go ps roleName =
        let binding = Binding roleName ps
        in binding : concatMap (go (roleName : ps)) (getRoles roleName)
    getRoles n = Set.toList (Map.findWithDefault Set.empty n plays)
    containedPlayers =
        toList (Set.unions (fmap (players sys) (containedRoles core sys)))


-- | The @PlaysRelation@ is the inverse of the @boundto@ relation. In
-- a given key-value-pair, the key represents the player, and the value
-- a list of roles played by this player.
type PlaysRelation = Map ComponentName (Set ComponentName)


buildPlaysRelation :: System -> PlaysRelation
buildPlaysRelation = foldr insertRole Map.empty . Map.assocs . view boundTo
  where
    insertRole (role, player) =
        Map.insertWith Set.union player (Set.singleton role)


-- | Gets the set of all core components in the system.  A core component is a
-- component that is not bound to another component.
coreComponents :: System -> Set ComponentName
coreComponents sys = Set.filter isCore (Map.keysSet (view instances sys))
  where
    isCore name = has (boundTo.at name._Nothing) sys


-- | Transitively get all roles bound to the given core component.
rolesOfCore :: PlaysRelation -> ComponentName -> Set RoleName
rolesOfCore plays core =
    Set.unions (fmap (`flatten` Set.empty) (Set.toList (getRoles core)))
  where
    flatten n roles = Set.insert n (Set.foldr flatten roles (getRoles n))
    getRoles n = Map.findWithDefault Set.empty n plays


-- | A pair of roles that must be compatible.
data CompatPair = CompatPair RoleName RoleName deriving (Eq, Ord, Show)

mkCompatPair :: RoleName -> RoleName -> CompatPair
mkCompatPair x y
    | x < y     = CompatPair x y
    | otherwise = CompatPair y x


checkCompatibilities
    :: System -> PlaysRelation -> Alphabets -> ComponentName -> Result ()
checkCompatibilities sys plays as core = do
    let roleNames = rolesOfCore plays core
        errors = concatMap checkCompatPair (compatPairs sys plays roleNames)
    if null errors
        then return ()
        else throwMany errors
  where
    checkCompatPair (CompatPair l r) =
        let lAlphabet = Map.findWithDefault Set.empty l as
            rAlphabet = Map.findWithDefault Set.empty r as
        in fmap (incompatError l r) (incompatibilities lAlphabet rAlphabet)

    incompatError first second (act, firstRgn, secondRgn) = locError firstRgn $
        IncompatibleRoles
            (componentName first)
            secondRgn
            (componentName second)
            (componentName core)
            (renderPretty act)


-- | Given a set of roles, get the set of all pairs of roles that must be
-- compatible.
compatPairs :: System -> PlaysRelation -> Set RoleName -> Set CompatPair
compatPairs sys plays roleNames =
    Set.unions (fmap compatPairsFor (Set.toList roleNames))
  where
    compatPairsFor roleName = Set.map (mkCompatPair roleName) $
        roleNames `Set.difference`
        (players sys roleName `Set.union` rolesOfCore plays roleName)


incompatibilities :: Alphabet -> Alphabet -> [(Action, Region, Region)]
incompatibilities l r = Set.toList (incompat l r `Set.union` incompat r l)
  where
    incompat l' r' = Set.unions (fmap (`joinWith` r') (overrideActs l'))

    overrideActs =
        Set.toList . Set.map actionName . Set.filter isOverrideAction

    joinWith act = Set.map (getLocs act) . Set.filter ((act ==) . actionName)

    getLocs (Loc act lRgn) actR = (act, lRgn, getLoc (actionName actR))


players :: System -> ComponentName -> Set ComponentName
players sys = go
  where
    go name = Set.insert name $ case view (boundTo.at name) sys of
        Just name' -> players sys name'
        Nothing    -> Set.empty
