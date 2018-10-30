{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


module Rbsc.Translator.Coordinator.Internal
    ( coordinatedRoles
    , rolesInConstraint
    ) where


import Control.Lens
import Control.Monad.Except

import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as Set


import Rbsc.Data.Component
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))


coordinatedRoles :: MonadError Error m => TCoordinator Elem -> m (Set RoleName)
coordinatedRoles Coordinator{..} =
    Set.unions <$> traverse rolesInConstraint constraints
  where
    constraints = mapMaybe (coordConstraint . getElem) coordCommands


rolesInConstraint
    :: MonadError Error m => TPlayingConstraint -> m (Set RoleName)
rolesInConstraint (PlayingConstraint (Loc (SomeExpr e _) _) roles) = do
    rolesInExpr <- Set.fromList . catMaybes <$>
        traverse getRoleFromExpr (universeExpr e)
    roles' <- Set.fromList <$> traverse getRole roles
    return (rolesInExpr `Set.union` roles')
  where
    getRoleFromExpr :: MonadError Error m => Some Expr -> m (Maybe RoleName)
    getRoleFromExpr = \case
        Some (IsPlayed (Loc (Literal comp _) _)) ->
            return (Just (view compName comp))
        Some (IsPlayed (Loc _ rgn)) ->
            throw rgn NotConstant
        _ -> return Nothing

    getRole :: MonadError Error m => LSomeExpr -> m RoleName
    getRole = \case
        Loc (SomeExpr (Literal comp (TyComponent _)) _) _ ->
            return (view compName comp)
        Loc _ rgn -> throw rgn NotConstant
