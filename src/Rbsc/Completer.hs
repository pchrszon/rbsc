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
import Control.Monad.State.Strict

import           Data.Char       (isUpper)
import           Data.Foldable   (for_)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (isNothing, mapMaybe)
import           Data.Monoid
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T


import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Util.NameGen


-- | Completion of an instance can lead to cycles if the same type is
-- encountered twice (which potentially leads to an infinite recursion).
type Cycle = [TypeName]


-- | A @Completer@ can nondeterministically choose between different
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


runCompleter :: Completer () -> SymbolTable -> System -> [Either Cycle System]
runCompleter m symTable sys =
    let results =
            runExceptT (execStateT (runReaderT m []) (CompletionState gen sys))
    in over (traverse._Right) (view system) results
  where
    gen = mkNameGen deriveFromTypeIdent takenNames
    takenNames = Set.map (\(ScopedName _ name) -> name) (Map.keysSet symTable)


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
completeSystem ::
       (MonadReader r m, HasSymbolTable r, HasComponentTypes r)
    => System
    -> m [Either Cycle System]
completeSystem sys = do
    types    <- view componentTypes
    symTable <- view symbolTable
    return (runCompleter (completeAll types) symTable sys)
  where
    completeAll types =
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
       ComponentTypes -> Name -> [[RoleRef]] -> Completer ()
completeCompartmentInstance types name roleRefLists = do
    sys <- use system

    let missings = missingRoles (containedRoles name sys) sys roleRefLists

    -- Only attempt to complete a compartment if none of its alternatives
    -- are complete already.
    unless (null missings || any null missings) $ do
        missing <- liftList missings

        for_ missing $ \roleTyName -> do
            roleName <- getOrCreateInstance types roleTyName

            -- check if the role is already contained in another compartment
            ci <- use $ system.containedIn
            liftList (guard (Map.notMember roleName ci))

            system.containedIn.at roleName .= Just name


-- | Get an instance of the given type or create a new one.
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


-- | @missingRoles contained sys roleRefLists@ returns a list of missing
-- roles for each possible instantiation of the @roleRefLists@.
missingRoles :: [RoleName] -> System -> [[RoleRef]] -> [[TypeName]]
missingRoles contained sys roleRefLists = nub $ do
    required <- roleRefLists
    requireds <- traverse fromRoleRef required

    let numRequired   = Map.unionsWith (+) requireds
        numContained' = Map.intersection numContained numRequired
        stillRequired = Map.unionWith (-) numRequired numContained'

    return (concatMap unfold (Map.toList stillRequired))
  where
    fromRoleRef (RoleRef tyName (lower, upper)) = do
        i <- [lower .. upper]
        return (Map.singleton tyName i)

    unfold (tyName, i) = replicate i tyName

    numContained =
        Map.unionsWith (+) (fmap (`Map.singleton` (1 :: Int)) containedTypes)

    containedTypes = mapMaybe (\name -> view (instances.at name) sys) contained

    nub = Set.toDescList . Set.fromList


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
