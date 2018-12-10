{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Instantiation of component implementations.
module Rbsc.Translator.Instantiation
    ( instantiateComponents
    , instantiateComponent
    , instantiateCoordinator
    , instantiateRewardStruct
    ) where


import Control.Lens
import Control.Monad
import Control.Monad.Except

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import qualified Data.Set         as Set
import           Data.Traversable


import Rbsc.Data.Component
import Rbsc.Data.Field
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Indices

import Rbsc.Util (withConstants)


-- | Instantiate the 'ModuleBody's for all 'Component's in the given
-- 'System'.
instantiateComponents
    :: (MonadEval r m, Has SymbolTable r, Has RangeTable r)
    => Model
    -> System
    -> m (Map ComponentName [TModuleInstance Elem])
instantiateComponents m sys =
    fmap Map.fromList (traverse inst (toComponents sys))
  where
    inst comp = do
        bodies <- instantiateComponent m comp
        bodies' <- for bodies $ \(ModuleInstance name args body) ->
            withConstants args $ do
                body' <-
                    reduceModuleBody =<< removeVariableIndicesInModule comp body
                return (ModuleInstance name args body')
        return (view compName comp, bodies')


-- | Instantiate a 'Coordinator'.
instantiateCoordinator
    :: (MonadEval r m, Has SymbolTable r, Has RangeTable r)
    => TCoordinator ElemMulti
    -> m (TCoordinator Elem)
instantiateCoordinator =
    reduceCoordinator <=< removeVariableIndicesInCoord <=< unrollCoordinator


-- | Instantiate a 'RewardStruct'.
instantiateRewardStruct
    :: MonadEval r m => TRewardStruct ElemMulti -> m (TRewardStruct Elem)
instantiateRewardStruct = reduceRewardStruct <=< unrollRewardStruct


reduceModuleBody :: MonadEval r m => TModuleBody Elem -> m (TModuleBody Elem)
reduceModuleBody ModuleBody {..} = do
    vars <- (traverse._2._Just) (exprs reduce) bodyVars
    cmds <- traverse (exprs reduce) bodyCommands
    return (ModuleBody vars cmds)


reduceCoordinator :: MonadEval r m => TCoordinator Elem -> m (TCoordinator Elem)
reduceCoordinator Coordinator {..} = do
    vars <- (traverse._2._Just) (exprs reduce) coordVars
    cmds <- traverse (exprs reduce) coordCommands
    return (Coordinator vars cmds)


reduceRewardStruct
    :: MonadEval r m => TRewardStruct Elem -> m (TRewardStruct Elem)
reduceRewardStruct RewardStruct {..} = RewardStruct rsName
    <$> traverse (exprs reduce) rsItems


-- | Instantiate all 'ModuleBody's for the given 'Component'.
instantiateComponent
    :: (MonadEval r m, Has SymbolTable r)
    => Model
    -> Component
    -> m [TModuleInstance Elem]
instantiateComponent m comp =
    traverse (instantiateModuleBody comp) insts
  where
    insts = fromMaybe [] (Map.lookup (view compTypeName comp) (modelImpls m))


instantiateModuleBody
    :: (MonadEval r m, Has SymbolTable r)
    => Component
    -> TModuleInstance ElemMulti
    -> m (TModuleInstance Elem)
instantiateModuleBody comp (ModuleInstance name args body) =
    withConstants args $ do
        body' <- substituteSelf comp body
        ModuleInstance name args <$> unrollModuleBody body'


unrollModuleBody ::
       MonadEval r m => TModuleBody ElemMulti -> m (TModuleBody Elem)
unrollModuleBody ModuleBody {..} = do
    cmds <- unrollElemMultis instantiateCommand bodyCommands
    cmds' <- for cmds $ \(Elem cmd) -> Elem <$> unrollCommand cmd
    return (ModuleBody bodyVars cmds')


unrollCoordinator
    :: MonadEval r m => TCoordinator ElemMulti -> m (TCoordinator Elem)
unrollCoordinator Coordinator {..} = do
    cmds  <- unrollElemMultis instantiateCoordCommand coordCommands
    cmds' <- for cmds $ \(Elem cmd) -> Elem <$> unrollCoordCommand cmd
    return (Coordinator coordVars cmds')


unrollCommand :: MonadEval r m => TCommand ElemMulti -> m (TCommand Elem)
unrollCommand Command {..} = do
    upds  <- unrollElemMultis instantiateUpdate cmdUpdates
    upds' <- for upds $ \(Elem upd) -> Elem <$> unrollUpdate upd
    return (Command cmdAction cmdActionKind cmdActionIntent cmdGuard upds')


unrollCoordCommand
    :: MonadEval r m => TCoordCommand ElemMulti -> m (TCoordCommand Elem)
unrollCoordCommand CoordCommand {..} = do
    upds <- unrollElemMultis instantiateUpdate coordUpdates
    upds' <- for upds $ \(Elem upd) -> Elem <$> unrollUpdate upd
    return (CoordCommand coordAction coordConstraint coordGuard upds')


unrollUpdate :: MonadEval r m => TUpdate ElemMulti -> m (TUpdate Elem)
unrollUpdate Update {..} =
    Update updProb <$> unrollElemMultis instantiateAssignment updAssignments


unrollRewardStruct
    :: MonadEval r m => TRewardStruct ElemMulti -> m (TRewardStruct Elem)
unrollRewardStruct RewardStruct {..} = RewardStruct rsName
    <$> unrollElemMultis instantiateRewardStructItem rsItems


unrollElemMultis
    :: MonadEval r m => Instantiate a -> [TElemMulti a] -> m [TElem a]
unrollElemMultis inst = fmap concat . traverse (unrollElemMulti inst)


unrollElemMulti :: MonadEval r m => Instantiate a -> TElemMulti a -> m [TElem a]
unrollElemMulti inst = \case
    ElemSingle x   -> return [Elem x]
    ElemLoop l     -> unrollLoop l
    ElemIf g elems -> case g of
        Loc (SomeExpr g' TyBool) rgn -> do
            b <- eval (Loc g' rgn)
            if b
                then unrollElemMultis inst elems
                else return []
        _ -> error "unrollElemMulti: type error"
  where
    unrollLoop Loop{..} = do
        consts <- view constants
        es <- case loopType of
            QdTypeComponent tySet -> return (componentConsts tySet consts)
            QdTypeInt (lower, upper) -> do
                l <- eval lower
                u <- eval upper
                return (fmap (\i -> SomeExpr (Literal i TyInt) TyInt) [l .. u])
        let bodies' =
                concatMap (\e -> instantiateElemMultis inst 0 e loopBody) es
        unrollElemMultis inst bodies'


-- | An instantiation function. The first argument gives the number of
-- binders we are operating under.
type Instantiate a = Int -> SomeExpr -> a -> a


instantiateCommand :: Instantiate (TCommand ElemMulti)
instantiateCommand i s Command{..} = Command
    { cmdAction       = fmap (instantiateLSomeExpr i s) cmdAction
    , cmdActionKind   = cmdActionKind
    , cmdActionIntent = cmdActionIntent
    , cmdGuard        = instantiateLSomeExpr i s cmdGuard
    , cmdUpdates      = instantiateElemMultis instantiateUpdate i s cmdUpdates
    }


instantiateCoordCommand :: Instantiate (TCoordCommand ElemMulti)
instantiateCoordCommand i s CoordCommand{..} = CoordCommand
    { coordAction     = fmap inst coordAction
    , coordConstraint = fmap (instantiatePlayingConstraint i s) coordConstraint
    , coordGuard      = inst coordGuard
    , coordUpdates    = instantiateElemMultis instantiateUpdate i s coordUpdates
    }
  where
    inst = instantiateLSomeExpr i s


instantiatePlayingConstraint :: Instantiate TPlayingConstraint
instantiatePlayingConstraint i s PlayingConstraint{..} = PlayingConstraint
    { pcExpr  = instantiateLSomeExpr i s pcExpr
    , pcRoles = fmap (instantiateLSomeExpr i s) pcRoles
    }


instantiateUpdate :: Instantiate (TUpdate ElemMulti)
instantiateUpdate i s Update{..} = Update
    { updProb        = fmap (instantiateLSomeExpr i s) updProb
    , updAssignments =
        instantiateElemMultis instantiateAssignment i s updAssignments
    }


instantiateElemMultis :: Instantiate a -> Instantiate [TElemMulti a]
instantiateElemMultis instInner i s = fmap (instantiateElemMulti instInner i s)


instantiateElemMulti :: Instantiate a -> Instantiate (TElemMulti a)
instantiateElemMulti instInner i s = \case
    ElemSingle x -> ElemSingle (instInner i s x)
    ElemLoop (Loop var ty body) ->
        let body' = fmap (instantiateElemMulti instInner (i + 1) s) body
        in ElemLoop (Loop var ty body')
    ElemIf c elems ->
        let c' = instantiateLSomeExpr i s c
            elems' = fmap (instantiateElemMulti instInner i s) elems
        in ElemIf c' elems'


instantiateAssignment :: Instantiate TAssignment
instantiateAssignment i s (Assignment name idxs e) =
    Assignment name (fmap inst idxs) (inst e)
  where
    inst = instantiateLSomeExpr i s


instantiateRewardStructItem :: Instantiate TRewardStructItem
instantiateRewardStructItem i s RewardStructItem{..} = RewardStructItem
    { riKind   = instantiateRewardKind i s riKind
    , riGuard  = inst riGuard
    , riReward = inst riReward
    }
  where
    inst = instantiateLSomeExpr i s


instantiateRewardKind :: Instantiate TRewardKind
instantiateRewardKind i s = \case
    TransitionReward mAct mConstr -> TransitionReward
        (fmap (instantiateLSomeExpr i s) mAct)
        (fmap (instantiatePlayingConstraint i s) mConstr)
    StateReward -> StateReward


instantiateLSomeExpr :: Instantiate LSomeExpr
instantiateLSomeExpr i s (Loc (SomeExpr e ty) rgn) =
    Loc (SomeExpr (instantiateUnder i (Scoped e) s) ty) rgn


-- | Replace the 'Self' keyword with a concrete instance.
substituteSelf
    :: MonadError Error m
    => Component
    -> TModuleBody ElemMulti
    -> m (TModuleBody ElemMulti)
substituteSelf comp ModuleBody{..} = do
    vars <- (traverse._2._Just) (transformExprsM subst) bodyVars
    cmds <- traverse (transformExprsM subst) bodyCommands
    return (ModuleBody vars cmds)
  where
    subst :: MonadError Error m => Expr t -> m (Expr t)
    subst e = case e of
        Self -> return (Literal comp
            (TyComponent (Set.singleton (view compTypeName comp))))

        _ -> return e
