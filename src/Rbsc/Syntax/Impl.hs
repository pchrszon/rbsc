{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}


-- | Abstract syntax of component implementations.
module Rbsc.Syntax.Impl
    ( Implementation(..)
    , ImplBody(..)
    , ModuleRef(..)

    , Module(..)
    , ModuleBody(..)
    , ModuleInstance(..)
    , miName
    , miArgs
    , miBody

    , Command(..)
    , cmdGuardLens
    , Update(..)
    , Assignment(..)

    , Elem(..)
    , ElemMulti(..)
    , Loop(..)
    ) where


import Control.Lens

import Data.Function
import Data.List.NonEmpty (NonEmpty)
import Data.Ord


import Rbsc.Data.Action
import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.Function
import Rbsc.Syntax.Typed.Expr


-- | An implementation of a component.
data Implementation elem vars ty expr = Implementation
    { implTypeName :: Loc TypeName
    , implBody     :: ImplBody elem vars ty expr
    }

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (Implementation ElemMulti vars ty expr)

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (Implementation Elem vars ty expr)


-- | An 'Implementation' can either reference a list of modules or directly
-- provide the implementation.
data ImplBody elem vars ty expr
    = ImplSingle (ModuleBody elem vars ty expr)
    | ImplModules (NonEmpty (ModuleRef expr))

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (ImplBody ElemMulti vars ty expr)

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (ImplBody Elem vars ty expr)


-- | Reference to a module with optional arguments for instantiation.
data ModuleRef expr = ModuleRef
    { modRefName :: Loc Name
    , modRefArgs :: [expr]
    } deriving (Show)


-- | A module describing the operational behavior of a component.
data Module elem vars ty expr = Module
    { modName   :: Loc Name
    , modParams :: [Parameter expr]
    , modBody   :: ModuleBody elem vars ty expr
    }

instance Eq (Module elem vars ty expr) where
    (==) = (==) `on` modName

instance Ord (Module elem vars ty expr) where
    compare = comparing modName

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (Module ElemMulti vars ty expr)

deriving instance
         (Show vars, Show ty, Show expr) => Show (Module Elem vars ty expr)


-- | The body of a module.
data ModuleBody elem vars ty expr = ModuleBody
    { bodyVars     :: vars
    , bodyCommands :: [elem ty expr (Command elem ty expr)]
    }

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (ModuleBody ElemMulti vars ty expr)

deriving instance
         (Show vars, Show ty, Show expr) =>
         Show (ModuleBody Elem vars ty expr)


-- | A module body together with its arguments.
data ModuleInstance elem vars ty expr arg = ModuleInstance
    { _miName :: !Name
    , _miArgs :: [arg]
    , _miBody :: ModuleBody elem vars ty expr
    }

deriving instance
         (Show vars, Show ty, Show expr, Show arg) =>
         Show (ModuleInstance ElemMulti vars ty expr arg)

deriving instance
         (Show vars, Show ty, Show expr, Show arg) =>
         Show (ModuleInstance Elem vars ty expr arg)


-- | A guarded command.
data Command elem ty expr = Command
    { cmdAction       :: Maybe expr
    , cmdActionKind   :: !ActionKind
    , cmdActionIntent :: !ActionIntent
    , cmdGuard        :: expr
    , cmdUpdates      :: [elem ty expr (Update elem ty expr)]
    }

deriving instance (Show ty, Show expr) => Show (Command ElemMulti ty expr)
deriving instance (Show ty, Show expr) => Show (Command Elem ty expr)

instance (HasExprs ty, HasExprs expr) =>
         HasExprs (Command ElemMulti ty expr) where
    exprs f Command {..} = Command
        <$> traverse (exprs f) cmdAction
        <*> pure cmdActionKind
        <*> pure cmdActionIntent
        <*> exprs f cmdGuard
        <*> traverse (exprs f) cmdUpdates

instance HasExprs expr => HasExprs (Command Elem ty expr) where
    exprs f Command {..} = Command
        <$> traverse (exprs f) cmdAction
        <*> pure cmdActionKind
        <*> pure cmdActionIntent
        <*> exprs f cmdGuard
        <*> traverse (exprs f) cmdUpdates


-- | A stochastic update.
data Update elem ty expr = Update
    { updProb        :: Maybe expr
    , updAssignments :: [elem ty expr (Assignment expr)]
    }

deriving instance (Show ty, Show expr) => Show (Update ElemMulti ty expr)
deriving instance (Show ty, Show expr) => Show (Update Elem ty expr)

instance (HasExprs ty, HasExprs expr) =>
         HasExprs (Update ElemMulti ty expr) where
    exprs f Update {..} =
        Update <$> traverse (exprs f) updProb <*>
        traverse (exprs f) updAssignments

instance HasExprs expr => HasExprs (Update Elem ty expr) where
    exprs f Update {..} =
        Update <$> traverse (exprs f) updProb <*>
        traverse (exprs f) updAssignments


-- | An assignment to a (possibly indexed) variable.
data Assignment expr = Assignment (Loc Name) [expr] expr deriving (Show)

instance HasExprs expr => HasExprs (Assignment expr) where
    exprs f (Assignment name idxs e) =
        Assignment name <$> traverse (exprs f) idxs <*> exprs f e


-- | An @Elem ty expr a@ is a single element of type @a@.
newtype Elem ty expr a = Elem { getElem :: a } deriving (Show)

instance HasExprs a => HasExprs (Elem ty expr a) where
    exprs f (Elem x) = Elem <$> exprs f x


-- | A @ElemMulti ty expr a@ is either a single element of type @a@,
-- a @forall@ block or an @if@ block.
data ElemMulti ty expr a
    = ElemSingle a
    | ElemLoop (Loop ty expr a)
    | ElemIf expr [ElemMulti ty expr a]
    deriving (Show)

instance (HasExprs ty, HasExprs expr, HasExprs a) => HasExprs (ElemMulti ty expr a) where
    exprs f = \case
        ElemSingle x   -> ElemSingle <$> exprs f x
        ElemLoop l     -> ElemLoop <$> exprs f l
        ElemIf e elems -> ElemIf <$> exprs f e <*> traverse (exprs f) elems


-- | @Loop a ty expr@ represents a @forall@ block surrounding a body of type
-- @a@. @ty@ is the type representing component types.
data Loop ty expr a = Loop
    { loopVar  :: Loc Name
    , loopType :: ty
    , loopBody :: [ElemMulti ty expr a]
    } deriving (Show)

instance (HasExprs ty, HasExprs expr, HasExprs a) =>
         HasExprs (Loop ty expr a) where
    exprs f Loop {..} =
        Loop loopVar <$> exprs f loopType <*> traverse (exprs f) loopBody


makeLenses ''ModuleInstance
makeLensesFor [("cmdGuard", "cmdGuardLens")] ''Command
