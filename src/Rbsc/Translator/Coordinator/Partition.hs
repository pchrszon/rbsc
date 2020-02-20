{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Partitioning of role coordinators.
--
-- The commands (and with it the variables) of a coordinator can
-- potentially be partitioned. Two commands are in separate partitions if
--  * the sets of roles they coordinate are disjoint, and
--  * the sets of variables they update are disjoint.
--
-- Partitioning a coordinator leads to smaller role alphabets (i.e., the
-- set of coordinated roles) in each partition. Since the translation of a
-- coordinator command generates (in the worst case) 2^n PRISM commands
-- (where n is the number of coordinated roles), this transformation
-- potentially leads to more compact PRISM code.
module Rbsc.Translator.Coordinator.Partition
    ( partition
    ) where


import Control.Monad.Except

import           Data.Function
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as Set


import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Translator.Coordinator.Internal

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Util


-- [NOTE] coordinator partitioning
--
-- 1. Build an undirected graph.
--      - Nodes are coordinator commands.
--      - There is an edge between two commands if
--          (1) their sets of updated variables are overlapping, or
--          (2) their sets of coordinated roles are overlapping.
--
-- 2. Create a new coordinator for each connected component of the graph.


partition :: MonadError Error m => TCoordinator Elem -> m [TCoordinator Elem]
partition Coordinator {..} = do
    let iCmds = zipWith (Id . getElem) coordCommands [0 ..]

    g <- buildCommandGraph iCmds
    let parts  = fmap (fmap unId . Set.toList) (connectedComponents g)
        coords = fmap (genCoordinator coordVars) parts

    return $ if Set.null readOnlyVars
        then coords
        else readOnlyVarsCoord : coords
  where
    readOnlyVarsCoord = Coordinator (filterVariables readOnlyVars coordVars) []

    readOnlyVars = Set.fromList (fmap fst coordVars) `Set.difference` cmdVars

    cmdVars = Set.unions (fmap (updatedVariables . getElem) coordCommands)


type ICoordCommand = Id (TCoordCommand Elem)

type CommandGraph = Graph ICoordCommand


buildCommandGraph :: MonadError Error m => [ICoordCommand] -> m CommandGraph
buildCommandGraph cmds = do
    cmdsRoles <- mapAnnotateA (rolesInCommand . unId) cmds
    let roleToCmds = inverseIndex cmdsRoles
        edges      = fmap (genEdges roleToCmds) cmdsRoles ++ nonCoordEdges
    return (Map.fromListWith Set.union edges)
  where
    genEdges
        :: Map RoleName (Set ICoordCommand)
        -> (ICoordCommand, Set RoleName)
        -> (ICoordCommand, Set ICoordCommand)
    genEdges roleToCmds (cmd, roles) =
        let connOverVars = findConnected varToCmds (updatedVariables (unId cmd))
            connOverRoles = findConnected roleToCmds roles
        in  (cmd, Set.union connOverVars connOverRoles)

    varToCmds :: Map Name (Set ICoordCommand)
    varToCmds = inverseIndex (mapAnnotate (updatedVariables . unId) cmds)

    -- create edges between non-coordination commands to keep them in the same
    -- coordinator
    nonCoordEdges :: [(ICoordCommand, Set ICoordCommand)]
    nonCoordEdges =
        concatMap (\cmd -> zip (repeat cmd) (fmap Set.singleton nonCoordCmds))
        nonCoordCmds

    nonCoordCmds = filter (isNothing .  coordConstraint . unId) cmds

    findConnected invIdx =
        Set.unions .
        fmap (\x -> Map.findWithDefault Set.empty x invIdx) .
        Set.toList


genCoordinator :: TInits -> [TCoordCommand Elem] -> TCoordinator Elem
genCoordinator vars cmds = Coordinator
    { coordVars     = filterVariables cmdVars vars
    , coordCommands = fmap Elem cmds
    }
  where
    cmdVars = Set.unions (fmap updatedVariables cmds)


updatedVariables :: TCoordCommand Elem -> Set Name
updatedVariables CoordCommand {..} =
    Set.unions (fmap (fromUpdate . getElem) coordUpdates)
  where
    fromUpdate Update {..} =
        Set.fromList (fmap (fromAssignment . getElem) updAssignments)

    fromAssignment (Assignment (Loc name _) _ _) = name


filterVariables :: Set Name -> TInits -> TInits
filterVariables vars = filter contained
  where
    contained (name, _) = name `Set.member` vars


rolesInCommand :: MonadError Error m => TCoordCommand Elem -> m (Set RoleName)
rolesInCommand CoordCommand {..} = case coordConstraint of
    Just c  -> rolesInConstraint c
    Nothing -> return Set.empty


-- Since CoordCommands are not comparable, the 'Id'-type is used to tag
-- each command with an index. Using this wrapper, we can use CoordCommands
-- in Sets and Maps.

data Id a = Id
    { unId  :: a
    , getId :: Integer
    } deriving (Show)

instance Eq (Id a) where
    (==) = (==) `on` getId

instance Ord (Id a) where
    compare = comparing getId
