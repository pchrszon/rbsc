{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Instantiation of component implementations.
module Rbsc.Translator.Instantiation
    ( instantiateComponents
    , instantiateComponent
    ) where


import Control.Lens

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import           Data.Traversable


import Rbsc.Data.Component
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

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
instantiateComponents m = fmap Map.fromList . traverse inst . toComponents
  where
    inst comp = do
        bodies <- instantiateComponent m comp
        bodies' <- for bodies $ \(NamedModuleBody name body) -> do
            body' <- reduceModuleBody =<< removeVariableIndices comp body
            return (NamedModuleBody name body')
        return (view compName comp, bodies')


reduceModuleBody :: MonadEval r m => TModuleBody Elem -> m (TModuleBody Elem)
reduceModuleBody ModuleBody {..} = do
    vars <- (traverse._2._Just) (exprs reduce) bodyVars
    cmds <- traverse (exprs reduce) bodyCommands
    return (ModuleBody vars cmds)


-- | Instantiate all 'ModuleBody's for the given 'Component'.
instantiateComponent ::
       MonadEval r m => Model -> Component -> m [TNamedModuleBody Elem]
instantiateComponent m comp = traverse (instantiateModuleBody comp) bodies
  where
    bodies = fromMaybe [] (Map.lookup (view compTypeName comp) (modelImpls m))


instantiateModuleBody ::
       MonadEval r m
    => Component
    -> TNamedModuleBody ElemMulti
    -> m (TNamedModuleBody Elem)
instantiateModuleBody comp (NamedModuleBody name body) =
    NamedModuleBody name <$> unrollModuleBody (substituteSelf comp body)


unrollModuleBody ::
       MonadEval r m => TModuleBody ElemMulti -> m (TModuleBody Elem)
unrollModuleBody ModuleBody{..} = do
    cmds <- unrollElemMultis bodyCommands
    cmds' <- for cmds $ \(Elem cmd) -> Elem <$> unrollCommand cmd
    return (ModuleBody bodyVars cmds')


unrollCommand :: MonadEval r m => TCommand ElemMulti -> m (TCommand Elem)
unrollCommand Command{..} = do
    upds <- unrollElemMultis cmdUpdates
    upds' <- for upds $ \(Elem upd) -> Elem <$> unrollUpdate upd
    return (Command cmdAction cmdActionKind cmdGuard upds')


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
        es <-
            case loopType of
                QdTypeComponent tySet -> return (componentConsts tySet consts)
                QdTypeInt (lower, upper) -> do
                    l <- eval lower
                    u <- eval upper
                    return (fmap (\i -> SomeExpr (Literal i) TyInt) [l .. u])
        let bodies' = concatMap (\e -> fmap (instantiateExprs e) loopBody) es
        unrollElemMultis bodies'


substituteSelf :: Component -> TModuleBody ElemMulti -> TModuleBody ElemMulti
substituteSelf comp ModuleBody{..} =
    ModuleBody bodyVars (fmap (transformExprs subst) bodyCommands)
  where
    subst :: Expr t -> Expr t
    subst e = case e of
        Self -> Literal comp
        _    -> e
