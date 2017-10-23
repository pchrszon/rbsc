{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}


module Rbsc.TypeChecker
    ( Typed(..)
    , typeCheck
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import           Data.List                 (find)
import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc (pretty)

import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region     (Loc (..), Region)

import           Rbsc.Syntax.Expr.Typed
import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.Component
import Rbsc.ComponentType
import Rbsc.Name
import Rbsc.SymbolTable
import Rbsc.Type


-- | A value tagged with a 'Type' @t@ where @t@ is existentially
-- quantified.
data Typed a where
    Typed :: Type t -> a t -> Typed a


data TcInfo = TcInfo
    { _componentTypes :: !ComponentTypes
    , _symbolTable    :: !SymbolTable
    , _boundVars      :: [(Name, AType)]
    }

makeLenses ''TcInfo


type TypeChecker a = ReaderT TcInfo (Either Type.Error) a


runTypeChecker ::
       TypeChecker a -> ComponentTypes -> SymbolTable -> Either Type.Error a
runTypeChecker m types symTable = runReaderT m (TcInfo types symTable [])


-- | Type check an untyped expression and transform it into a typed
-- expression.
typeCheck ::
       ComponentTypes
    -> SymbolTable
    -> Loc U.Expr
    -> Either Type.Error (Typed Expr)
typeCheck types symTable e = runTypeChecker (tc e) types symTable


tc :: Loc U.Expr -> TypeChecker (Typed Expr)
tc (Loc e rgn) = case e of
    U.LitBool b ->
        Literal b `withType` TyBool

    U.Variable name ->
        lookupBoundVar name >>= \case
            Just (i, AType ty) -> do
                Refl <- expect tyComponent rgn ty
                Bound i `withType` ty
            Nothing -> do
                AType ty <- getIdentifierType name rgn
                Variable name ty `withType` ty

    U.Not inner -> do
        inner' <- inner `hasType` TyBool
        Not inner' `withType` TyBool

    U.BoolBinOp binOp l r -> do
        l' <- l `hasType` TyBool
        r' <- r `hasType` TyBool
        BoolBinOp binOp l' r' `withType` TyBool

    U.HasType inner tyName ->
        whenTypeExists tyName $ do
            inner' <- inner `hasType` tyComponent
            HasType inner' (unLoc tyName) `withType` TyBool

    U.BoundTo l r -> do
        l' <- l `hasType` tyComponent
        r' <- r `hasType` tyComponent
        BoundTo l' r' `withType` TyBool

    U.Element l r -> do
        l' <- l `hasType` tyComponent
        r' <- r `hasType` tyComponent
        Element l' r' `withType` TyBool

    U.Quantified q varName mTyName body -> do
        varTy <- case mTyName of
            Just tyName -> whenTypeExists tyName $
                return (TyComponent (Just (unLoc tyName)) undefined) -- TODO: get local variables
            Nothing ->
                return (TyComponent Nothing Map.empty)

        body' <- local (over boundVars ((varName, AType varTy) :)) $
            body `hasType` TyBool

        Quantified q (fmap unLoc mTyName) (Scope body') `withType` TyBool


-- | Looks up the type of a given identifier in the symbol table. If the
-- identifier is undefined, an error is thrown.
getIdentifierType :: Name -> Region -> TypeChecker AType
getIdentifierType name rgn = do
    varTy <- view (symbolTable.at name)
    case varTy of
        Just ty -> return ty
        Nothing -> throwError (Type.UndefinedIdentifier rgn)


-- | Looks up the type and the de-Bruijn index of a given identifier.
lookupBoundVar :: Name -> TypeChecker (Maybe (Int, AType))
lookupBoundVar name = do
    vars <- view boundVars
    let indexedVars = zip vars [0..]
    return (fmap toIndexAndType (lookupVar name indexedVars))
  where
    lookupVar n = find ((n ==) . fst . fst)
    toIndexAndType ((_, aTy), i) = (i, aTy)


whenTypeExists :: Loc TypeName -> TypeChecker a -> TypeChecker a
whenTypeExists (Loc tyName rgn) m = do
    types <- view componentTypes
    if Map.member tyName types
        then m
        else throwError (Type.UndefinedType rgn)


-- | Assume that a given untyped expression has a given 'Type'.
hasType :: Loc U.Expr -> Type t -> TypeChecker (Expr t)
hasType e expected = do
    Typed actual e' <- tc e
    Refl <- expect expected (getLoc e) actual
    return e'


-- | @expect expected rgn actual@ returns a witness that the types
-- @expected@ and @actual@ are equal (w.r.t. 'typeEq').
expect :: Type s -> Region -> Type t -> TypeChecker (s :~: t)
expect expected rgn actual =
    case typeEq expected actual of
        Just Refl -> return Refl
        Nothing ->
            throwError
                (Type.TypeError (renderType expected) (renderType actual) rgn)
  where
    renderType :: Type r -> Text.Text
    renderType = Text.pack . show . pretty


-- | Returns an expression tagged with its 'Type'.
withType :: Expr t -> Type t -> TypeChecker (Typed Expr)
withType e ty = return (Typed ty e)


tyComponent :: Type Component
tyComponent = TyComponent Nothing Map.empty
