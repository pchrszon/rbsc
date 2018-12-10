{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}


-- | This module provides the @TypeChecker@ monad and various utility
-- functions for type checking.
module Rbsc.TypeChecker.Internal
    ( -- * TypeChecker monad
      TypeChecker

    , TcInfo
    , componentTypes
    , symbolTable
    , boundVars
    , scope
    , context
    , TcContext(..)

    , runTypeChecker

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
import Rbsc.Data.Field
import Rbsc.Data.ModelInfo
import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..), Region)
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed.Expr (Constants, SomeExpr (..), constants,
                                         methods)
import qualified Rbsc.Syntax.Typed.Expr as T

import Rbsc.Util (renderPretty)


-- | The @TypeChecker@ monad.
type TypeChecker a = ReaderT TcInfo Result a


-- | The type checking context.
data TcContext
    = NoContext -- ^ Standard type checking. No special rules apply.
    | ActionContext -- ^ In the action context, all undefined identifiers are treated as action names.
    | ConstraintContext -- ^ In the role-constraint context, all components are treated as bool.


-- | The list of variables bound by a quantifier or lambda.
type BoundVars = [(Name, Some Type)]


-- | The information provided to the type checker.
type TcInfo =
    ComponentTypes :&:
    TypeSets :&:
    SymbolTable :&:
    Constants :&:
    Methods :&:
    RecursionDepth :&:
    [(Name, Some Type)] :&: -- list of variables bound by a quantifier or lambda
    Scope :&: -- the current scope
    TcContext -- the current type checking context


boundVars :: Lens' TcInfo BoundVars
boundVars = field


scope :: Lens' TcInfo Scope
scope = field


context :: Lens' TcInfo TcContext
context = field


-- | Run a 'TypeChecker' action.
runTypeChecker
    :: ( Has ComponentTypes r
       , Has TypeSets r
       , Has SymbolTable r
       , Has Constants r
       , Has Methods r
       , Has RecursionDepth r
       )
    => TypeChecker a
    -> r
    -> Result a
runTypeChecker m r = runReaderT m
    (   view componentTypes r
    :&: view typeSets r
    :&: view symbolTable r
    :&: view constants r
    :&: view methods r
    :&: view recursionDepth r
    :&: []
    :&: Global
    :&: NoContext
    )


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
