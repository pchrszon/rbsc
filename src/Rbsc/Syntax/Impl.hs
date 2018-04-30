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


import Rbsc.Syntax.Quantification
import Rbsc.Syntax.VarDecl


-- | An implementation of a component.
data Implementation comp expr = Implementation
    { implTypeName :: Loc TypeName
    , implBody     :: ImplBody comp expr
    } deriving (Show)


-- | An 'Implementation' can either reference a list of modules or directly
-- provide the implementation.
data ImplBody comp expr
    = ImplSingle (ModuleBody comp expr)
    | ImplModules (NonEmpty (Loc Name))
    deriving (Show)


-- | A module describing the operational behavior of a component.
data Module comp expr = Module
    { modName :: Loc Name
    , modBody :: ModuleBody comp expr
    } deriving (Show)


-- | The body of a module.
data ModuleBody comp expr = ModuleBody
    { bodyVars     :: [VarDecl expr]
    , bodyCommands :: Body (Command comp) comp expr
    } deriving (Show)


-- | A guarded command.
data Command comp expr = Command
    { cmdAction  :: Maybe expr
    , cmdGuard   :: expr
    , cmdUpdates :: Body (Update comp) comp expr
    } deriving (Show)


-- | A stochastic update.
data Update comp expr = Update
    { updProb :: Maybe expr
    , updAssignments :: Body Assignment comp expr
    } deriving (Show)


-- | An assignment to a (possibly indexed) variable.
data Assignment expr = Assignment (Loc Name) [expr] expr deriving (Show)


-- | A @Body@ consists of a list of @BodyItem@s.
newtype Body a comp expr = Body [BodyItem a comp expr] deriving (Show)


-- | A @BodyItem a comp expr@ is either a single item of type @a@,
-- a @forall@ block or an @if@ block.
data BodyItem a comp expr
    = ItemSingle (a expr)
    | ItemLoop (Loop (Body a comp) comp expr)
    | ItemIf expr (Body a comp expr)
    deriving (Show)


-- | @Loop a comp expr@ represents a @forall@ block surrounding a body of type
-- @a@. @comp@ is the type representing component types.
data Loop a comp expr = Loop
    { loopVar  :: Loc Name
    , loopType :: QuantifiedType comp expr
    , loopBody :: a expr
    } deriving (Show)
