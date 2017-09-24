{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}


-- | Generation of minimal complete system instances.
module Instantiation
    (
    -- * Types
      Types
    , TypeName(..)
    , Type(..)

    -- * Instances
    , InstanceName(..)
    , RoleInstanceName

    -- * System Instances
    , System(..)
    , instances
    , boundTo
    , containedIn

    , instancesOfType
    , boundRoles
    , containedRoles

    -- * System Completion
    , Cycle
    , completeSystem
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import           Data.Char                 (isUpper)
import           Data.Foldable             (find, for_)
import           Data.List                 (delete)
import           Data.Map                  (Map, assocs)
import qualified Data.Map                  as Map
import           Data.Maybe                (isNothing)
import           Data.Monoid
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc hiding ((<>))

import Util
import Util.NameGen


-- | A collection of (named) types.
type Types = Map TypeName Type


-- | The name of a user-defined component type, role type or compartment type.
newtype TypeName = TypeName
    { getTypeName :: Text
    } deriving (Eq, Ord, Show)

instance Pretty TypeName where
    pretty = pretty . getTypeName


-- | A type.
data Type
    = TypeComponent
    | TypeCompartment [TypeName]
    | TypeRole [TypeName]
    deriving (Show)



-- | The name of a component instance, role instance or compartment instance.
newtype InstanceName = InstanceName
    { getInstanceName :: Text
    } deriving (Eq, Ord, Show)

instance Pretty InstanceName where
    pretty = pretty . getInstanceName


-- | An 'InstanceName' that is intended to be a role instance name.
type RoleInstanceName = InstanceName


-- | A system instance that assigns a type to each component, role and
-- compartment. Also specifies role bindings and compartment membership.
--
-- A system instance may be incomplete, i.e., roles may be unbound and
-- compartments may be missing certain role instances.
data System = System
    { _instances   :: Map InstanceName TypeName
    , _boundTo     :: Map RoleInstanceName InstanceName
    , _containedIn :: Map RoleInstanceName InstanceName
    } deriving (Show)

makeLenses ''System

instance Pretty System where
    pretty sys = sep (punctuate comma (instanceDocs ++ bindingDocs))
      where
        instanceDocs =
            fmap (prettyInstance sys) (view (instances.to assocs) sys)
        bindingDocs =
            fmap prettyBinding .
            filter (not . null . snd) .
            fmap (\i -> (i, boundRoles i sys)) .
            view (instances.to Map.keys) $ sys

prettyInstance :: System -> (InstanceName, TypeName) -> Doc ann
prettyInstance sys (name, tyName) =
    pretty name <+>
    colon <+>
    pretty tyName <>
    if null contained
        then emptyDoc
        else space <> hang 0 (parens (commaSep contained))
  where
    contained = containedRoles name sys

prettyBinding :: (InstanceName, [RoleInstanceName]) -> Doc ann
prettyBinding (name, roleNames) =
    pretty name <+> "boundto" <+> hang 0 (parens (commaSep roleNames))

commaSep :: Pretty a => [a] -> Doc ann
commaSep = sep . punctuate comma . fmap pretty


-- | Get all instances that have the given type.
instancesOfType :: TypeName -> System -> [InstanceName]
instancesOfType tyName = inverseLookup tyName . view instances


-- | Get all roles that are bound to a given player.
boundRoles :: InstanceName -> System -> [RoleInstanceName]
boundRoles name = inverseLookup name . view boundTo


-- | Get all roles contained in a given compartment.
containedRoles :: InstanceName -> System -> [RoleInstanceName]
containedRoles name = inverseLookup name . view containedIn


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
    taken = Set.map getInstanceName (sys ^. instances . to Map.keysSet)


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
completeSystem :: Types -> System -> [Either Cycle System]
completeSystem types sys = runCompleter completeAll sys
  where
    completeAll =
        ifor_ (view instances sys) $ \name tyName -> do
            completeInstance types name tyName
            -- discard all system instances containing cycles
            liftList . guard . isNothing . getCycle =<< use system


-- | Complete the given instance.
completeInstance :: Types -> InstanceName -> TypeName -> Completer ()
completeInstance types name tyName = unlessVisited tyName $
    case Map.lookup tyName types of
        Just TypeComponent -> return ()
        Just (TypeRole playerTyNames) ->
            completeRoleInstance types name playerTyNames
        Just (TypeCompartment roleTyNames) ->
            completeCompartmentInstance types name roleTyNames
        -- non-existing type names are handled by semantic check
        Nothing -> return ()


-- | Complete a role by binding it to a player if necessary.
completeRoleInstance :: Types -> InstanceName -> [TypeName] -> Completer ()
completeRoleInstance types name playerTyNames =
    use (system.boundTo.at name) >>= \case
        Just _ -> return () -- the role is already bound
        Nothing -> do
            playerTyName <- liftList playerTyNames
            player <- getOrCreateInstance types playerTyName
            system.boundTo.at name .= Just player


-- | Complete a compartment instance by adding a role instance for each
-- required role type.
completeCompartmentInstance ::
       Types -> InstanceName -> [TypeName] -> Completer ()
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
getOrCreateInstance :: Types -> TypeName -> Completer InstanceName
getOrCreateInstance types tyName = do
    create <- liftList [True, False]
    if create
        then createInstance types tyName
        else getInstance tyName


-- | Create a new instance of the given type and add it to the system. The
-- returned instance is already complete.
createInstance :: Types -> TypeName -> Completer InstanceName
createInstance types tyName = do
    name <- InstanceName <$> newNameFrom (getTypeName tyName)
    completeInstance types name tyName
    system.instances.at name .= Just tyName
    return name


-- | Get an existing instance for the given type name.
getInstance :: TypeName -> Completer InstanceName
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
missingRoles :: [RoleInstanceName] -> [TypeName] -> System -> [TypeName]
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
getCycle :: System -> Maybe [InstanceName]
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
