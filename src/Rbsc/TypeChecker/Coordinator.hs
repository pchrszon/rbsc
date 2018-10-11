{-# LANGUAGE RecordWildCards #-}


module Rbsc.TypeChecker.Coordinator
    ( tcCoordinator
    ) where


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
    <*> tcElemMultis tcCoordCommand coordCommands


tcCoordCommand :: UCoordCommand -> TypeChecker (TCoordCommand ElemMulti)
tcCoordCommand CoordCommand{..} = CoordCommand
    <$> traverse (\act -> (`withLocOf` act) <$> tcAction act) coordAction
    <*> traverse tcPlayingConstraint coordConstraint
    <*> someExpr coordGuard TyBool
    <*> tcElemMultis tcUpdate coordUpdates


tcPlayingConstraint :: UPlayingConstraint -> TypeChecker TPlayingConstraint
tcPlayingConstraint PlayingConstraint{..} = do
    constr <- tcRoleConstraint pcExpr
    roles  <- traverse tcRoleExpr pcRoles
    return (PlayingConstraint (constr `withLocOf` pcExpr) roles)
