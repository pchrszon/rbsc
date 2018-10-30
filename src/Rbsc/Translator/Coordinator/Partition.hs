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


import Control.Lens
import Control.Monad.Except

import Data.Traversable
import           Data.Function
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import           Data.Ord
import           Data.Set        (Set)
import qualified Data.Set        as Set


import Rbsc.Data.Name

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Coordinator.Internal

import Rbsc.Util (regions)


-- [NOTE] coordinator partitioning
--
-- 1. The coordinator commands are partitioned into "regions", where each
--    region updates the same set of variables. Since each command can
--    update multiple variables, two regions can be joined by adding
--    a third command. Consider:
--
--      [][...] ... -> (x' = 0);
--      [][...] ... -> (y' = 1);
--      [][...] ... -> (x' = 2) & (y' = 3);
--
--    If the third command didn't exist, the first two commands would be in
--    separate regions.
--
-- 2. If there is a region with no variables, the commands in this region
--    are further partitioned w.r.t. the roles they coordinate. These
--    commands can be put into different modules without any restrictions,
--    since they update no variables.
--
-- 3. If regions from (1) and partitions of (2) coordinate the same set of
--    roles, they are merged.
--
-- 4. A coordinator for each partition is generated.


partition :: MonadError Error m => TCoordinator Elem -> m [TCoordinator Elem]
partition coord@Coordinator{..} = do
    statelessCoords <- fmap concat . for statelessPart $
        fmap (fmap (over _2 genStatelessCoordinator)) . partitionOnRoleSets . snd

    statefulCoords <- for statefulParts $ \part -> do
        let coord' = genStatefulCoordinator coord part
        roles <- coordinatedRoles coord'
        return (roles, coord')

    return
        (Map.elems (Map.fromListWith (<>) (statelessCoords ++ statefulCoords)))
  where
    cmdVars =
        fmap ((\cmd -> (cmd, updatedVariables cmd)) . getElem) coordCommands

    varPartitions = partitionOnVariables cmdVars

    (statelessPart, statefulParts) =
        List.partition (Set.null . fst) varPartitions


genStatefulCoordinator
    :: TCoordinator Elem
    -> (Set Name, [TCoordCommand Elem])
    -> TCoordinator Elem
genStatefulCoordinator Coordinator {..} (vars, cmds) = Coordinator
    { coordVars     = filterVariables vars coordVars
    , coordCommands = fmap Elem cmds
    }


genStatelessCoordinator :: [TCoordCommand Elem] -> TCoordinator Elem
genStatelessCoordinator = Coordinator [] . fmap Elem


filterVariables :: Set Name -> TInits -> TInits
filterVariables vars = filter contained
  where
    contained (name, _) = name `Set.member` vars


partitionOnRoleSets
    :: MonadError Error m
    => [TCoordCommand Elem]
    -> m [(Set RoleName, [TCoordCommand Elem])]
partitionOnRoleSets =
    fmap (Map.assocs . Map.fromListWith (++)) . traverse addRoleSet
  where
    addRoleSet cmd = do
        roleSet <- roles cmd
        return (roleSet, [cmd])

    roles cmd = case coordConstraint cmd of
        Just pc -> rolesInConstraint pc
        Nothing -> return Set.empty


partitionOnVariables
    :: [(TCoordCommand Elem, Set Name)]
    -> [(Set Name, [TCoordCommand Elem])]
partitionOnVariables cmds = over (traverse._2) removeIndices (regions cmds')
  where
    cmds' = Map.fromList (fmap addIndex (zip cmds [0 ..]))
    addIndex ((x, y), i) = (Id (x, i), y)
    removeIndices = fmap (fst . unId) . Set.toList


-- Since CoordCommands are not comparable, the 'Id'-type is used to tag
-- each command with an index. Using this wrapper, we can use CoordCommands
-- in Sets and Maps.

newtype Id a = Id { unId :: (a, Integer) }

instance Eq (Id a) where
    (==) = (==) `on` (snd . unId)

instance Ord (Id a) where
    compare = comparing (snd . unId)


updatedVariables :: TCoordCommand Elem -> Set Name
updatedVariables CoordCommand {..} =
    Set.unions (fmap (fromUpdate . getElem) coordUpdates)
  where
    fromUpdate Update {..} =
        Set.fromList (fmap (fromAssignment . getElem) updAssignments)

    fromAssignment (Assignment (Loc name _) _ _) = name
