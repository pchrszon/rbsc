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
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Indices


-- | Instantiate the 'ModuleBody's for all 'Component's in the given
-- 'System'.
instantiateComponents ::
       (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => Model
    -> System
    -> m (Map Name [TNamedModuleBody Elem])
instantiateComponents m sys =
    fmap Map.fromList (traverse inst (toComponents sys))
  where
    inst comp = do
        bodies <- instantiateComponent m sys comp
        bodies' <- for bodies $ \(NamedModuleBody name body) -> do
            body' <-
                reduceModuleBody =<< removeVariableIndicesInModule comp body
            return (NamedModuleBody name body')
        return (view compName comp, bodies')


-- | Instantiate a 'Coordinator'.
instantiateCoordinator
    :: (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => TCoordinator ElemMulti
    -> m (TCoordinator Elem)
instantiateCoordinator =
    reduceCoordinator <=< removeVariableIndicesInCoord <=< unrollCoordinator


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


-- | Instantiate all 'ModuleBody's for the given 'Component'.
instantiateComponent
    :: MonadEval r m
    => Model
    -> System
    -> Component
    -> m [TNamedModuleBody Elem]
instantiateComponent m sys comp =
    traverse (instantiateModuleBody sys comp) bodies
  where
    bodies = fromMaybe [] (Map.lookup (view compTypeName comp) (modelImpls m))


instantiateModuleBody ::
       MonadEval r m
    => System
    -> Component
    -> TNamedModuleBody ElemMulti
    -> m (TNamedModuleBody Elem)
instantiateModuleBody sys comp (NamedModuleBody name body) = do
    body' <- substituteKeywords sys comp body
    NamedModuleBody name <$> unrollModuleBody body'


unrollModuleBody ::
       MonadEval r m => TModuleBody ElemMulti -> m (TModuleBody Elem)
unrollModuleBody ModuleBody {..} = do
    cmds <- unrollElemMultis bodyCommands
    cmds' <- for cmds $ \(Elem cmd) -> Elem <$> unrollCommand cmd
    return (ModuleBody bodyVars cmds')


unrollCoordinator
    :: MonadEval r m => TCoordinator ElemMulti -> m (TCoordinator Elem)
unrollCoordinator Coordinator {..} = do
    cmds  <- unrollElemMultis coordCommands
    cmds' <- for cmds $ \(Elem cmd) -> Elem <$> unrollCoordCommand cmd
    return (Coordinator coordVars cmds')


unrollCommand :: MonadEval r m => TCommand ElemMulti -> m (TCommand Elem)
unrollCommand Command {..} = do
    upds  <- unrollElemMultis cmdUpdates
    upds' <- for upds $ \(Elem upd) -> Elem <$> unrollUpdate upd
    return (Command cmdAction cmdActionKind cmdGuard upds')


unrollCoordCommand
    :: MonadEval r m => TCoordCommand ElemMulti -> m (TCoordCommand Elem)
unrollCoordCommand CoordCommand {..} = do
    upds <- unrollElemMultis coordUpdates
    upds' <- for upds $ \(Elem upd) -> Elem <$> unrollUpdate upd
    return (CoordCommand coordAction coordConstraint coordGuard upds')


unrollUpdate :: MonadEval r m => TUpdate ElemMulti -> m (TUpdate Elem)
unrollUpdate Update{..} = Update updProb <$> unrollElemMultis updAssignments


unrollElemMultis :: (HasExprs a, MonadEval r m) => [TElemMulti a] -> m [TElem a]
unrollElemMultis = fmap concat . traverse unrollElemMulti


unrollElemMulti :: (HasExprs a, MonadEval r m) => TElemMulti a -> m [TElem a]
unrollElemMulti = \case
    ElemSingle x   -> return [Elem x]
    ElemLoop l     -> unrollLoop l
    ElemIf g elems -> case g of
        Loc (SomeExpr g' TyBool) rgn -> do
            b <- eval (Loc g' rgn)
            if b
                then unrollElemMultis elems
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
        let bodies' = concatMap (\e -> fmap (instantiateExprs e) loopBody) es
        unrollElemMultis bodies'


-- | Replace the 'Self' and 'Player' keywords with concrete instances.
substituteKeywords
    :: MonadError Error m
    => System
    -> Component
    -> TModuleBody ElemMulti
    -> m (TModuleBody ElemMulti)
substituteKeywords sys comp ModuleBody {..} = do
    cmds' <- traverse (transformExprsM subst) bodyCommands
    return (ModuleBody bodyVars cmds')
  where
    subst :: MonadError Error m => Expr t -> m (Expr t)
    subst e = case e of
        Self -> return (Literal comp
            (TyComponent (Set.singleton (view compTypeName comp))))
        Player rgn -> case view compBoundTo comp of
            Just playerName -> do
                let playerComp = toComponent playerName
                    ty = TyComponent (Set.singleton (_compTypeName playerComp))
                return (Literal playerComp ty)
            Nothing -> throw rgn (UndefinedPlayer (view compName comp))
        _ -> return e

    toComponent name = case view (instances.at name) sys of
        Just tyName -> Component
            { _compName        = name
            , _compTypeName    = tyName
            , _compBoundTo     = view (boundTo.at name) sys
            , _compContainedIn = view (containedIn.at name) sys
            }
        Nothing -> error $
            "substituteKeywords: undefined component " ++ show name
