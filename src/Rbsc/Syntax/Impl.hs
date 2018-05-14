-- | Abstract syntax of component implementations.
module Rbsc.Syntax.Impl
    ( Implementation(..)
    , ImplBody(..)

    , Module(..)
    , ModuleBody(..)

    , Command(..)
    , Update(..)
    , Assignment(..)

    , Body(..)
    , BodyItem(..)
    , Loop(..)
    ) where


import Data.List.NonEmpty (NonEmpty)


import Rbsc.Data.Name
import Rbsc.Report.Region


-- | An implementation of a component.
data Implementation vars ty expr = Implementation
    { implTypeName :: Loc TypeName
    , implBody     :: ImplBody vars ty expr
    } deriving (Show)


-- | An 'Implementation' can either reference a list of modules or directly
-- provide the implementation.
data ImplBody vars ty expr
    = ImplSingle (ModuleBody vars ty expr)
    | ImplModules (NonEmpty (Loc Name))
    deriving (Show)


-- | A module describing the operational behavior of a component.
data Module vars ty expr = Module
    { modName :: Loc Name
    , modBody :: ModuleBody vars ty expr
    } deriving (Show)


-- | The body of a module.
data ModuleBody vars ty expr = ModuleBody
    { bodyVars     :: vars
    , bodyCommands :: Body (Command ty) ty expr
    } deriving (Show)


-- | A guarded command.
data Command ty expr = Command
    { cmdAction  :: Maybe expr
    , cmdGuard   :: expr
    , cmdUpdates :: Body (Update ty) ty expr
    } deriving (Show)


-- | A stochastic update.
data Update ty expr = Update
    { updProb        :: Maybe expr
    , updAssignments :: Body Assignment ty expr
    } deriving (Show)


-- | An assignment to a (possibly indexed) variable.
data Assignment expr = Assignment (Loc Name) [expr] expr deriving (Show)


-- | A @Body@ consists of a list of @BodyItem@s.
newtype Body a ty expr = Body [BodyItem a ty expr] deriving (Show)


-- | A @BodyItem a ty expr@ is either a single item of type @a@,
-- a @forall@ block or an @if@ block.
data BodyItem a ty expr
    = ItemSingle (a expr)
    | ItemLoop (Loop (Body a ty) ty expr)
    | ItemIf expr (Body a ty expr)
    deriving (Show)


-- | @Loop a ty expr@ represents a @forall@ block surrounding a body of type
-- @a@. @ty@ is the type representing component types.
data Loop a ty expr = Loop
    { loopVar  :: Loc Name
    , loopType :: ty
    , loopBody :: a expr
    } deriving (Show)
