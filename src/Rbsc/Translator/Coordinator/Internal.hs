{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


module Rbsc.Translator.Coordinator.Internal
    ( coordinatedRoles
    , rolesInConstraint
    , Valuation
    , satisfyingValuations
    , allValuatations
    ) where


import Control.Lens
import Control.Monad.Except

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import GHC.Exts (IsList (..))


import Rbsc.Data.Component
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Eval

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
    roles' <- Set.fromList <$> getRoles roles
    return (rolesInExpr `Set.union` roles')
  where
    getRoleFromExpr :: MonadError Error m => Some Expr -> m (Maybe RoleName)
    getRoleFromExpr = \case
        Some (IsPlayed (Loc (Literal comp _) _)) ->
            return (Just (view compName comp))
        Some (IsPlayed (Loc _ rgn)) ->
            throw rgn NotConstant
        _ -> return Nothing

    getRoles :: MonadError Error m => Maybe LSomeExpr -> m [RoleName]
    getRoles = \case
        Nothing -> return []
        Just (Loc (SomeExpr (Literal arr (TyArray _ (TyComponent _))) _) _) ->
            return (fmap (view compName) (toList arr))
        Just (Loc _ rgn) -> throw rgn NotConstant


type Valuation = Map RoleName Bool


satisfyingValuations
    :: MonadEval r m => [Valuation] -> Loc (Expr Bool) -> m [Valuation]
satisfyingValuations valuations constraint = filterM satisfying valuations
  where
    satisfying val = eval (fmap (substituteRoles val) constraint)

    substituteRoles :: Valuation -> Expr t -> Expr t
    substituteRoles val = transformExpr $ \case
        IsPlayed (Loc (Literal comp (TyComponent _)) _) ->
            case Map.lookup (view compName comp) val of
                Just b -> Literal b TyBool
                Nothing -> error $
                    "satisfyingValuations: " ++ show comp ++ " not found"
        e -> e


allValuatations :: Set RoleName -> [Valuation]
allValuatations = go . Set.toList
  where
    go (roleName : roleNames) = do
        val <- go roleNames
        b <- [True, False]
        return (Map.insert roleName b val)
    go [] = return Map.empty
