{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


-- | Abstract syntax of role-playing coordinators.
module Rbsc.Syntax.Coordinator
    ( Coordinator(..)
    , CoordCommand(..)
    , coordGuardLens
    , PlayingConstraint(..)
    ) where


import Control.Lens


import Rbsc.Syntax.Impl
import Rbsc.Syntax.Typed.Expr


-- The coordinator module.
data Coordinator elem vars ty expr = Coordinator
    { coordVars     :: vars
    , coordCommands :: [elem ty expr (CoordCommand elem ty expr)]
    }

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (Coordinator ElemMulti vars ty expr)

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (Coordinator Elem vars ty expr)


-- | A coordinator command possibly having a role-playing constraint.
data CoordCommand elem ty expr = CoordCommand
    { coordAction     :: Maybe expr
    , coordConstraint :: Maybe (PlayingConstraint expr)
    , coordGuard      :: expr
    , coordUpdates    :: [elem ty expr (Update elem ty expr)]
    }

deriving instance (Show ty, Show expr) => Show (CoordCommand ElemMulti ty expr)
deriving instance (Show ty, Show expr) => Show (CoordCommand Elem ty expr)

instance (HasExprs ty, HasExprs expr) =>
         HasExprs (CoordCommand ElemMulti ty expr) where
    exprs f CoordCommand {..} = CoordCommand
        <$> traverse (exprs f) coordAction
        <*> traverse (exprs f) coordConstraint
        <*> exprs f coordGuard
        <*> traverse (exprs f) coordUpdates

instance HasExprs expr => HasExprs (CoordCommand Elem ty expr) where
    exprs f CoordCommand {..} = CoordCommand
        <$> traverse (exprs f) coordAction
        <*> traverse (exprs f) coordConstraint
        <*> exprs f coordGuard
        <*> traverse (exprs f) coordUpdates


-- | A role-playing constraint with a list of roles that are coordinated.
data PlayingConstraint expr = PlayingConstraint
    { pcExpr  :: expr
    , pcRoles :: [expr]
    } deriving (Show)

instance HasExprs expr => HasExprs (PlayingConstraint expr) where
    exprs f PlayingConstraint{..} = PlayingConstraint
        <$> exprs f pcExpr
        <*> traverse (exprs f) pcRoles


makeLensesFor [("coordGuard", "coordGuardLens")] ''CoordCommand
