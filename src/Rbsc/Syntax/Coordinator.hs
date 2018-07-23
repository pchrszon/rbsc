{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Abstract syntax of role-playing coordinators.
module Rbsc.Syntax.Coordinator
    ( Coordinator(..)
    , CoordCommand(..)
    ) where


import Rbsc.Syntax.Impl


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
    , coordConstraint :: Maybe expr
    , coordGuard      :: expr
    , coordUpdates    :: [elem ty expr (Update elem ty expr)]
    }

deriving instance (Show ty, Show expr) => Show (CoordCommand ElemMulti ty expr)
deriving instance (Show ty, Show expr) => Show (CoordCommand Elem ty expr)
