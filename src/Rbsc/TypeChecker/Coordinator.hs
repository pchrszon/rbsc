{-# LANGUAGE RecordWildCards #-}


module Rbsc.TypeChecker.Coordinator
    ( tcCoordinator
    ) where


import Data.Maybe (isJust)


import Rbsc.Data.Type

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
    roles  <- traverse tcRoleExpr pcRoles
    return (PlayingConstraint (constr `withLocOf` pcExpr) roles)
