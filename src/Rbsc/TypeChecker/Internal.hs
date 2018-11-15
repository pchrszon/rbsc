{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}


-- | This module provides the @TypeChecker@ monad and various utility
-- functions for type checking.
module Rbsc.TypeChecker.Internal
    ( -- * TypeChecker monad
      TypeChecker

    , TcInfo(..)
    , componentTypes
    , symbolTable
    , boundVars
    , scope
    , context
    , TcContext(..)

    , runTypeChecker
    , runTypeChecker'

    , getIdentifierType
    , lookupBoundVar
    , localScope
    , whenTypeExists

      -- * Extracting typed expressions
    , getExpr
    , extract

      -- * Utility functions
    , expect
    , withType
    , isEqType
    , isOrdType
    , isNumType
    , typeError
    ) where


import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import           Data.List       (find)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)


import Rbsc.Config

import Rbsc.Data.ComponentType
import Rbsc.Data.Name
import Rbsc.Data.ModelInfo
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..), Region)
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed.Expr (Constants, HasConstants, SomeExpr (..))
import qualified Rbsc.Syntax.Typed.Expr as T

import Rbsc.Util (renderPretty)


-- | The @TypeChecker@ monad.
type TypeChecker a = ReaderT TcInfo Result a


-- | The type checking context.
data TcContext
    = NoContext -- ^ Standard type checking. No special rules apply.
    | ActionContext -- ^ In the action context, all undefined identifiers are treated as action names.
    | ConstraintContext -- ^ In the role-constraint context, all components are treated as bool.


-- | The information provided to the type checker.
data TcInfo = TcInfo
    { _tciComponentTypes :: !ComponentTypes     -- ^ 'ComponentType' defined in the model
    , _tciTypeSets       :: !TypeSets           -- ^ the user defined component type sets
    , _tciSymbolTable    :: !SymbolTable        -- ^ the 'SymbolTable'
    , _tciConstants      :: !Constants          -- ^ the defined constants
    , _tciRecursionDepth :: !RecursionDepth     -- ^ the maximum recursion depth
    , _boundVars         :: [(Name, Some Type)] -- ^ list of variables bound by a quantifier or lambda
    , _scope             :: !Scope              -- ^ the current scope
    , _context           :: !TcContext          -- ^ the current type checking context
    }

makeLenses ''TcInfo

instance HasComponentTypes TcInfo where
    componentTypes = tciComponentTypes

instance HasTypeSets TcInfo where
    typeSets = tciTypeSets

instance HasSymbolTable TcInfo where
    symbolTable = tciSymbolTable

instance HasConstants TcInfo where
    constants = tciConstants

instance HasRecursionDepth TcInfo where
    recursionDepth = tciRecursionDepth


-- | Run a type checker action.
runTypeChecker :: TypeChecker a -> ModelInfo -> RecursionDepth -> Result a
runTypeChecker m info = runTypeChecker' m
    (view componentTypes info)
    (view typeSets info)
    (view symbolTable info)
    (view constants info)


-- | Run a type checker action. Since the type checker does not need a full
-- 'ModelInfo', use this function to supply only the necessary information.
runTypeChecker'
    :: TypeChecker a
    -> ComponentTypes
    -> TypeSets
    -> SymbolTable
    -> Constants
    -> RecursionDepth
    -> Result a
runTypeChecker' m types tySets symTable consts depth =
    runReaderT m (TcInfo types tySets symTable consts depth [] Global NoContext)


-- | Looks up the type of a given identifier in the symbol table. First,
-- the local scope is checked, then the global scope.  If the identifier is
-- undefined and the identifier does not appear within an action expression, an
-- error is thrown.
getIdentifierType :: Name -> Region -> TypeChecker (Some Type)
getIdentifierType name rgn = do
    sc  <- view scope
    ctx <- view context

    varTyLocal  <- view (symbolTable.at (ScopedName sc name))
    varTyGlobal <- view (symbolTable.at (ScopedName Global name))

    -- If we are inside action brackets, all undefined identifiers are
    -- actions.
    let varTyAction = case ctx of
            ActionContext -> Just (Some TyAction)
            _             -> Nothing

    case varTyLocal <|> varTyGlobal <|> varTyAction of
        Just ty -> return ty
        Nothing -> throw rgn UndefinedIdentifier


-- | Looks up the type and the de-Bruijn index of a given identifier.
lookupBoundVar :: Name -> TypeChecker (Maybe (Int, Some Type))
lookupBoundVar name = do
    vars <- view boundVars
    let indexedVars = zip vars [0..]
    return (fmap toIndexAndType (lookupVar name indexedVars))
  where
    lookupVar n = find ((n ==) . fst . fst)
    toIndexAndType ((_, aTy), i) = (i, aTy)


-- | Run a 'TypeChecker' in the local 'Scope' of the given compoent type.
localScope :: TypeName -> TypeChecker a -> TypeChecker a
localScope tyName = local (scope .~ Local tyName)


-- | When a given user-defined component type exists, execute the given
-- action. Otherwise, an error is thrown.
whenTypeExists :: Loc TypeName -> TypeChecker a -> TypeChecker a
whenTypeExists (Loc tyName rgn) m = do
    types <- view componentTypes
    if Map.member tyName types
        then m
        else throw rgn UndefinedType


-- | Unwrap 'SomeExpr'. If the given expected 'Type' and the actual @Type@ do
-- not match, then @Nothing@ is returned.
getExpr :: Type t -> SomeExpr -> Maybe (T.Expr t)
getExpr expected (SomeExpr e actual) = do
    Refl <- typeEq expected actual
    return e


-- | @extract expected region e@ extracts an expression @e@ wrapped in
-- 'SomeExpr'. If @e@ does not have the @expected@ type, a type error is
-- thrown.
extract :: MonadError Error m => Type t -> Region -> SomeExpr -> m (T.Expr t)
extract expected rgn (SomeExpr e actual) = do
    Refl <- expect expected rgn actual
    return e


-- | @expect expected rgn actual@ returns a witness that the types
-- @expected@ and @actual@ are equal (w.r.t. 'typeEq').
expect :: MonadError Error m => Type s -> Region -> Type t -> m (s :~: t)
expect expected rgn actual =
    case typeEq expected actual of
        Just Refl -> return Refl
        Nothing   -> throw rgn (typeError [Some expected] actual)


-- | Assume that values of the given type can be checked for equality. If
-- not, an error is thrown.
isEqType :: Type t -> Region -> TypeChecker (Dict (Eq t))
isEqType ty rgn = case checkEq ty of
    Just Dict -> return Dict
    Nothing   -> throw rgn (NotComparable (renderPretty ty))


-- | Assume that values of the given type are comparable. If not, an error
-- is thrown.
isOrdType :: Type t -> Region -> TypeChecker (Dict (Ord t))
isOrdType ty rgn = case checkOrd ty of
    Just Dict -> return Dict
    Nothing   -> throw rgn (NotComparable (renderPretty ty))


-- | Assume that the given type is a number type. If not, a type error is
-- thrown.
isNumType :: Type t -> Region -> TypeChecker (Dict (Num t))
isNumType ty rgn = case checkNum ty of
    Just Dict -> return Dict
    Nothing   -> throw rgn (typeError numTypes ty)


-- | Returns an expression tagged with its 'Type'.
withType :: T.Expr t -> Type t -> TypeChecker SomeExpr
withType e ty = return (SomeExpr e ty)


-- | @typeError expected actual region@ constructs a 'Type.Error'.
typeError :: [Some Type] -> Type t -> LocErrorDesc
typeError expected actual =
    TypeError (fmap renderSomeType expected) (renderPretty actual)
  where
    renderSomeType :: Some Type -> Text
    renderSomeType (Some ty) = renderPretty ty
