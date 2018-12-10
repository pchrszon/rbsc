{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RankNTypes            #-}


-- | Removal of variables used as indices.
--
-- If a variable is used inside index brackets, e.g., @arr[x]@, where @x@
-- is a variable, the index must be made constant. This is accomplished by
-- replicating the command for every possible value of @x@. Then, the guard
-- of the command is extended by @x = value@.
module Rbsc.Translator.Indices
    ( removeVariableIndicesInModule
    , removeVariableIndicesInCoord
    ) where


import Control.Lens
import Control.Monad.Reader


import Rbsc.Data.Component
import Rbsc.Data.Field
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Indices.Internal


-- | Remove all variables appearing inside indexing brackets by replacing
-- them with all possible values.
removeVariableIndicesInModule ::
       (MonadEval r m, Has SymbolTable r, Has RangeTable r)
    => Component
    -> TModuleBody Elem
    -> m (TModuleBody Elem)
removeVariableIndicesInModule comp (ModuleBody vars cmds) = ModuleBody vars
    <$> removeInCommands cmdGuardLens cmdUpdates (Just comp) cmds


-- | Remove all variables appearing inside indexing brackets by replacing
-- them with all possible values.
removeVariableIndicesInCoord
    :: (MonadEval r m, Has SymbolTable r, Has RangeTable r)
    => TCoordinator Elem
    -> m (TCoordinator Elem)
removeVariableIndicesInCoord (Coordinator vars cmds) = Coordinator vars
    <$> removeInCommands coordGuardLens coordUpdates Nothing cmds


removeInCommands
    :: ( MonadReader r m
       , Has Constants r
       , Has SymbolTable r
       , Has RangeTable r
       , HasExprs a
       )
    => Lens' a LSomeExpr
    -> (a -> Updates)
    -> Maybe Component
    -> [TElem a]
    -> m [TElem a]
removeInCommands guardLens getUpdates mComp = fmap concat . traverse
    (fmap (fmap Elem) . removeInCommand guardLens getUpdates mComp . getElem)


removeInCommand
    :: ( MonadReader r m
       , Has Constants r
       , Has SymbolTable r
       , Has RangeTable r
       , HasExprs a
       )
    => Lens' a LSomeExpr
    -> (a -> Updates)
    -> Maybe Component
    -> a
    -> m [a]
removeInCommand guardLens getUpdates mComp cmd = do
    ranges <- getIndexRanges getUpdates mComp cmd
    return (fmap remove (variableValues ranges))
  where
    remove vals =
        let cmd' = foldr substituteVariable cmd vals
            g = (case view guardLens cmd' of
                    Loc (SomeExpr e TyBool) _ -> e
                    _ -> error "removeVariableIndices: type error") :: Expr Bool
            g' = LogicOp And (valueGuard vals) g
        in set guardLens (SomeExpr g' TyBool `withLocOf` view guardLens cmd) cmd'
