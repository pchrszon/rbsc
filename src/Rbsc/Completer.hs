{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}


-- | Generation of minimal complete system instances.
module Rbsc.Completer
    ( Cycle
    , completeSystem
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import           Data.Char       (isUpper)
import           Data.Foldable   (find, for_)
import           Data.List       (delete)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (isNothing)
import           Data.Monoid
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T

import Rbsc.System
import Rbsc.Type
import Rbsc.Util.NameGen


-- | Completion of an instance can lead to cycles if the same type is
-- encountered twice (which potentially leads to an infinite recursion).
type Cycle = [TypeName]


-- | A 'Completer' can nondeterministically select from different
-- alternatives to generate all possible system instances. A call stack is
-- used to detect cycles when creating new instances.
type Completer a = ReaderT Stack (StateT CompletionState (ExceptT Cycle [])) a

type Stack = [TypeName]

data CompletionState = CompletionState
    { _csNameGen :: NameGen
    , _system    :: System
    }

makeLenses ''CompletionState

instance HasNameGen CompletionState where
    nameGen = csNameGen


runCompleter :: Completer () -> System -> [Either Cycle System]
runCompleter m sys =
    let results =
            runExceptT (execStateT (runReaderT m []) (CompletionState gen sys))
    in over (traverse._Right) (view system) results
  where
    gen = mkNameGen deriveFromTypeIdent taken
    taken = view (instances.to Map.keysSet) sys


-- | Lift a nondeterministic choice into the 'Completer' monad.
liftList :: [a] -> Completer a
liftList = lift . lift . lift


-- | Completes the given system by completing all instances within the
-- system. Every possible instantiation of the system is returned.
--
-- A role instance is complete if it has a player. A compartment instance is
-- complete if there is a contained role instance for each required role type.
--
-- Some instantiations may lead to a cycle (e.g., a role played by
-- a compartment is contained within the compartment).
completeSystem :: ComponentTypes -> System -> [Either Cycle System]
completeSystem types sys = runCompleter completeAll sys
  where
    completeAll =
        ifor_ (view instances sys) $ \name tyName -> do
            completeInstance types name tyName
            -- discard all system instances containing cycles
            liftList . guard . isNothing . getCycle =<< use system


-- | Complete the given instance.
completeInstance :: ComponentTypes -> Name -> TypeName -> Completer ()
completeInstance types name tyName = unlessVisited tyName $
    case Map.lookup tyName types of
        Just NaturalType -> return ()
        Just (RoleType playerTyNames) ->
            completeRoleInstance types name playerTyNames
        Just (CompartmentType roleTyNames) ->
            completeCompartmentInstance types name roleTyNames
        -- non-existing type names are handled by semantic check
        Nothing -> return ()


-- | Complete a role by binding it to a player if necessary.
completeRoleInstance :: ComponentTypes -> Name -> Set TypeName -> Completer ()
completeRoleInstance types name playerTyNames =
    use (system.boundTo.at name) >>= \case
        Just _ -> return () -- the role is already bound
        Nothing -> do
            playerTyName <- liftList (Set.toList playerTyNames)
            player <- getOrCreateInstance types playerTyName
            system.boundTo.at name .= Just player


-- | Complete a compartment instance by adding a role instance for each
-- required role type.
completeCompartmentInstance ::
       ComponentTypes -> Name -> [TypeName] -> Completer ()
completeCompartmentInstance types name roleTyNames = do
    sys <- use system
    let missing = missingRoles (containedRoles name sys) roleTyNames sys

    for_ missing $ \roleTyName -> do
        roleName <- getOrCreateInstance types roleTyName

        -- check if the role is already contained in another compartment
        ci <- use $ system.containedIn
        liftList (guard (Map.notMember roleName ci))

        system.containedIn.at roleName .= Just name


-- | Create a new instance for the given type or create a new one.
getOrCreateInstance :: ComponentTypes -> TypeName -> Completer Name
getOrCreateInstance types tyName = do
    create <- liftList [True, False]
    if create
        then createInstance types tyName
        else getInstance tyName


-- | Create a new instance of the given type and add it to the system. The
-- returned instance is already complete.
createInstance :: ComponentTypes -> TypeName -> Completer Name
createInstance types tyName = do
    name <- newNameFrom (getTypeName tyName)
    completeInstance types name tyName
    system.instances.at name .= Just tyName
    return name


-- | Get an existing instance for the given type name.
getInstance :: TypeName -> Completer Name
getInstance tyName = liftList =<< use (system.to (instancesOfType tyName))


-- | Throws an error if the given type name has been encountered before.
-- Otherwise, the given action is executed.
unlessVisited
    :: (MonadReader Stack m, MonadError Cycle m)
    => TypeName -> m a -> m a
unlessVisited typeName m = do
    callStack <- ask
    if typeName `elem` callStack
        then throwError (reverse (typeName:callStack))
        else local (typeName:) m


-- | Given a list of role instances and a list of required role types, get
-- a list of all role types that have no corresponding instance.
missingRoles :: [RoleName] -> [TypeName] -> System -> [TypeName]
missingRoles contained required sys =
    evalState (filterM (fmap not . hasInstance) required) contained
  where
    hasInstance tyName = do
        cs <- get
        case find (hasType tyName) cs of
            Just name -> do
                modify (delete name)
                return True
            Nothing -> return False
    hasType tyName name = view (instances.at name) sys == Just tyName


-- | Get the first cycle induced by the 'boundTo' relation if it exists.
getCycle :: System -> Maybe [Name]
getCycle sys =
    getFirst (mconcat (fmap (go []) (view (instances.to Map.keys) sys)))
  where
    go visited name
        | name `elem` visited = First (Just visited')
        | otherwise =
            case view (boundTo.at name) sys of
                -- the current instance is a role: follow the boundTo
                -- relation to its player
                Just name' -> go visited' name'
                -- the current instance is not a role, but it may be
                -- a compartment
                Nothing ->
                    mconcat (fmap (go visited') (containedRoles name sys))
      where
        visited' = name : visited


-- | Derive an instance name from a type name by taking only the uppercase
-- letters into account (works best if type names are in CamelCase).
deriveFromTypeIdent :: Text -> Text
deriveFromTypeIdent tyName =
    let name = T.toLower (T.filter isUpper tyName)
    in if T.null name
        then T.take 1 tyName
        else name
