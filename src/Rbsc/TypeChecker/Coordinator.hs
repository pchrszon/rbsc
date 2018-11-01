{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}


module Rbsc.TypeChecker.Coordinator
    ( tcCoordinator
    ) where


import Control.Lens

import qualified Data.Map.Strict as Map
import           Data.Maybe      (isJust)
import qualified Data.Set        as Set


import Rbsc.Data.ComponentType
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed   hiding (Model (..), Type (..))
import Rbsc.Syntax.Untyped hiding (Model (..), Type (..))

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Impl
import Rbsc.TypeChecker.Internal


tcCoordinator :: UCoordinator -> TypeChecker (TCoordinator ElemMulti)
tcCoordinator Coordinator{..} = Coordinator
    <$> traverse tcVarDecl coordVars
    <*> tcElemMultis (tcCoordCommand ownVars) coordCommands
  where
    ownVars = fmap (unLoc . declName) coordVars


tcCoordCommand
    :: [Name] -> UCoordCommand -> TypeChecker (TCoordCommand ElemMulti)
tcCoordCommand ownVars CoordCommand{..} = CoordCommand
    <$> traverse (\act -> (`withLocOf` act) <$> tcAction act) coordAction
    <*> traverse tcPlayingConstraint coordConstraint
    <*> someExpr coordGuard TyBool
    <*> tcElemMultis (tcUpdate hasAction ownVars) coordUpdates
  where
    hasAction = isJust coordAction || isJust coordConstraint


tcPlayingConstraint :: UPlayingConstraint -> TypeChecker TPlayingConstraint
tcPlayingConstraint PlayingConstraint{..} = do
    constr <- tcRoleConstraint pcExpr
    roles  <- traverse tcRoleArray pcRoles
    return (PlayingConstraint (constr `withLocOf` pcExpr) roles)


tcRoleArray :: LExpr -> TypeChecker LSomeExpr
tcRoleArray e = do
    let Loc _ rgn = e

    compTys <- view componentTypes
    let roleTys = Map.keysSet (Map.filter (has _RoleType) compTys)

    SomeExpr e' ty <- tcExpr e
    case ty of
        TyArray size (TyComponent tySet)
            | tySet `Set.isSubsetOf` roleTys ->
                return (Loc (SomeExpr e' ty) rgn)
            | otherwise -> throw rgn (typeError [expectedType size roleTys] ty)
        _ -> throw rgn (typeError [expectedType 1 roleTys] ty)
  where
    expectedType size roleTys = Some (TyArray size (TyComponent roleTys))
