{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}


module Rbsc.TypeChecker
    ( AnExpr(..)
    , getExpr

    , typeCheck
    , extract
    ) where


import Control.Lens         hiding ((<|))
import Control.Monad.Except
import Control.Monad.Reader

import           Data.List                 (find)
import           Data.List.NonEmpty        (NonEmpty (..), (<|))
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc (pretty)


import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.Function
import Rbsc.Data.Name
import Rbsc.Data.SymbolTable
import Rbsc.Data.Type

import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region     (Loc (..), Region)

import qualified Rbsc.Syntax.Expr.Typed   as T
import qualified Rbsc.Syntax.Expr.Untyped as U


-- | An 'Expr' tagged with its 'Type'.
data AnExpr where
    AnExpr :: T.Expr t -> Type t -> AnExpr


-- | Unwrap 'AnExpr'. If the given expected 'Type' and the actual @Type@ do
-- not match, then @Nothing@ is returned.
getExpr :: Type t -> AnExpr -> Maybe (T.Expr t)
getExpr expected (AnExpr e actual) = do
    Refl <- typeEq expected actual
    return e


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
       ComponentTypes -> SymbolTable -> Loc U.Expr -> Either Type.Error AnExpr
typeCheck types symTable e = runTypeChecker (tc e) types symTable


-- | @extract expected region e@ extracts an expression @e@ wrapped in
-- 'AnExpr'. If @e@ does not have the @expected@ type, a type error is
-- thrown.
extract :: Type t -> Region -> AnExpr -> Either Type.Error (T.Expr t)
extract expected rgn (AnExpr e actual) = do
    Refl <- expect expected rgn actual
    return e


tc :: Loc U.Expr -> TypeChecker AnExpr
tc (Loc e rgn) = case e of
    U.LitBool b ->
        T.Literal b `withType` TyBool

    U.LitInt i ->
        T.Literal i `withType` TyInt

    U.LitDouble d ->
        T.Literal d `withType` TyDouble

    U.Function f ->
        return (fromFunctionSym f)

    U.Array es ->
        tcArray es

    U.Variable name ->
        lookupBoundVar name >>= \case
            Just (i, AType ty) -> do
                Refl <- expect tyComponent rgn ty
                T.Bound i `withType` ty
            Nothing -> do
                AType ty <- getIdentifierType name rgn
                T.Variable name ty `withType` ty

    U.Not inner -> do
        inner' <- inner `hasType` TyBool
        T.Not inner' `withType` TyBool

    U.Negate inner -> do
        AnExpr inner' ty <- tc inner
        Dict <- isNumType ty (getLoc inner)
        T.Negate inner' `withType` ty

    U.ArithOp aOp l r -> do
        (AnExpr l' tyL, AnExpr r' tyR) <- binaryCast <$> tc l <*> tc r
        Dict <- isNumType tyL (getLoc l)
        _    <- isNumType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.ArithOp aOp l' r' `withType` tyL

    U.Divide l r -> do
        l' <- l `hasType` TyDouble
        r' <- r `hasType` TyDouble
        T.Divide rgn l' r' `withType` TyDouble

    U.EqOp eOp l r -> do
        (AnExpr l' tyL, AnExpr r' tyR) <- binaryCast <$> tc l <*> tc r
        Dict <- isEqType tyL (getLoc l)
        _    <- isEqType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.EqOp eOp l' r' `withType` TyBool

    U.RelOp rOp l r -> do
        (AnExpr l' tyL, AnExpr r' tyR) <- binaryCast <$> tc l <*> tc r
        Dict <- isOrdType tyL (getLoc l)
        _    <- isOrdType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.RelOp rOp l' r' `withType` TyBool

    U.LogicOp lOp l r -> do
        l' <- l `hasType` TyBool
        r' <- r `hasType` TyBool
        T.LogicOp lOp l' r' `withType` TyBool

    U.Index inner idx -> do
        AnExpr inner' ty <- tc inner
        case ty of
            TyArray elemTy _ -> do
                idx' <- idx `hasType` TyInt
                Dict <- return (dictShow elemTy)
                T.Index inner' (Loc idx' (getLoc idx)) `withType` elemTy
            _ -> throwError (Type.NotAnArray (renderType ty) (getLoc inner))

    U.Call f args -> do
        f' <- tc f
        checkCallArity rgn f' args
        tcCall (Loc f' (getLoc f)) args

    U.HasType inner tyName ->
        whenTypeExists tyName $ do
            inner' <- inner `hasType` tyComponent
            T.HasType inner' (unLoc tyName) `withType` TyBool

    U.BoundTo l r -> do
        l' <- l `hasType` tyComponent
        r' <- r `hasType` tyComponent
        T.BoundTo l' r' `withType` TyBool

    U.Element l r -> do
        l' <- l `hasType` tyComponent
        r' <- r `hasType` tyComponent
        T.Element l' r' `withType` TyBool

    U.Quantified q varName mTyName body -> do
        varTy <- case mTyName of
            Just tyName -> whenTypeExists tyName $
                return (TyComponent (Just (unLoc tyName)))
            Nothing -> return (TyComponent Nothing)

        body' <- local (over boundVars ((varName, AType varTy) :)) $
            body `hasType` TyBool

        T.Quantified q (fmap unLoc mTyName) (T.Scope body') `withType` TyBool


fromFunctionSym :: FunctionSym -> AnExpr
fromFunctionSym = \case
    FuncMinInt ->
        AnExpr (T.Function MinInt) (TyInt --> TyInt --> TyInt)
    FuncMinDouble ->
        AnExpr (T.Function MinDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMaxInt ->
        AnExpr (T.Function MaxInt) (TyInt --> TyInt --> TyInt)
    FuncMaxDouble ->
        AnExpr (T.Function MaxDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncFloor ->
        AnExpr (T.Function Floor) (TyDouble --> TyInt)
    FuncCeil ->
        AnExpr (T.Function Ceil) (TyDouble --> TyInt)
    FuncPowInt ->
        AnExpr (T.Function PowInt) (TyInt --> TyInt --> TyInt)
    FuncPowDouble ->
        AnExpr (T.Function PowDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMod ->
        AnExpr (T.Function Mod) (TyInt --> TyInt --> TyInt)
    FuncLog ->
        AnExpr (T.Function Log) (TyDouble --> TyDouble --> TyDouble)


checkCallArity :: Region -> AnExpr -> [args] -> TypeChecker ()
checkCallArity rgn (AnExpr _ ty) args
    | numParams > 0 && numParams < numArgs =
        throwError (Type.WrongNumberOfArguments numParams numArgs rgn)
    | otherwise = return ()
  where
    numParams = paramsLength ty
    numArgs   = length args

    paramsLength :: Type t -> Int
    paramsLength = \case
        TyFunc _ tyRes -> 1 + paramsLength tyRes
        _              -> 0


tcCall :: Loc AnExpr -> [Loc U.Expr] -> TypeChecker AnExpr
tcCall (Loc (AnExpr e ty) _) [] = return (AnExpr e ty)
tcCall (Loc (AnExpr f (TyFunc tyParam tyRes)) rgn) (arg : args) = do
    arg' <- arg `hasType` tyParam
    Dict <- return (dictShow tyRes)
    tcCall (Loc (AnExpr (T.Apply f arg') tyRes) rgn) args
tcCall (Loc (AnExpr _ ty) rgn) (_ : _) =
    throwError (Type.NotAFunction (renderType ty) rgn)


tcArray :: NonEmpty (Loc U.Expr) -> TypeChecker AnExpr
tcArray (e :| []) = do
    AnExpr e' ty <- tc e
    Dict <- return (dictShow ty)
    return (AnExpr (T.Array (e' :| [])) (TyArray ty (Just 1)))
tcArray (e :| (x:xs)) = do
    AnExpr e' ty <- tc e
    AnExpr (T.Array es') (TyArray elemTy len) <- tcArray (x :| xs)
    Refl <- expect elemTy (getLoc e) ty
    return (AnExpr (T.Array (e' <| es')) (TyArray ty (succ <$> len)))


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


-- | If one of the two given expression has 'TyDouble' and the other one
-- has 'TyInt', then cast the @TyInt@ expression to @TyDouble@.
binaryCast :: AnExpr -> AnExpr -> (AnExpr, AnExpr)
binaryCast (AnExpr l TyDouble) (AnExpr r TyInt) =
    (AnExpr l TyDouble, AnExpr (T.Cast r) TyDouble)
binaryCast (AnExpr l TyInt) (AnExpr r TyDouble) =
    (AnExpr (T.Cast l) TyDouble, AnExpr r TyDouble)
binaryCast l r = (l, r)


-- | @cast expected e@ casts expression @t@ to 'TyDouble' if @expected@ is
-- @TyDouble@ and the expression has type 'TyInt'.
cast :: Type t -> AnExpr -> AnExpr
cast TyDouble (AnExpr e TyInt) = AnExpr (T.Cast e) TyDouble
cast _        e                = e


-- | When a given user-defined component type exists, execute the given
-- action. Otherwise, an error is thrown.
whenTypeExists :: Loc TypeName -> TypeChecker a -> TypeChecker a
whenTypeExists (Loc tyName rgn) m = do
    types <- view componentTypes
    if Map.member tyName types
        then m
        else throwError (Type.UndefinedType rgn)


-- | Assume that a given untyped expression has a given 'Type'.
--
-- If the expected type is 'TyDouble' but the expression has type 'TyInt',
-- a 'Cast' is inserted automatically.
hasType :: Loc U.Expr -> Type t -> TypeChecker (T.Expr t)
hasType e expected = do
    AnExpr e' actual <- cast expected <$> tc e
    Refl <- expect expected (getLoc e) actual
    return e'


-- | @expect expected rgn actual@ returns a witness that the types
-- @expected@ and @actual@ are equal (w.r.t. 'typeEq').
expect :: MonadError Type.Error m => Type s -> Region -> Type t -> m (s :~: t)
expect expected rgn actual =
    case typeEq expected actual of
        Just Refl -> return Refl
        Nothing   -> throwError (typeError [AType expected] actual rgn)


-- | Assume that values of the given type can be checked for equality. If
-- not, an error is thrown.
isEqType :: Type t -> Region -> TypeChecker (Dict (Eq t))
isEqType ty rgn = case checkEq ty of
    Just Dict -> return Dict
    Nothing   -> throwError (Type.NotComparable (renderType ty) rgn)


-- | Assume that the given type is a number type. If not, a type error is
-- thrown.
isNumType :: Type t -> Region -> TypeChecker (Dict (Num t))
isNumType ty rgn = case checkNum ty of
    Just Dict -> return Dict
    Nothing   -> throwError (typeError numTypes ty rgn)


-- | Assume that values of the given type are comparable. If not, an error
-- is thrown.
isOrdType :: Type t -> Region -> TypeChecker (Dict (Ord t))
isOrdType ty rgn = case checkOrd ty of
    Just Dict -> return Dict
    Nothing   -> throwError (Type.NotComparable (renderType ty) rgn)


-- | Returns an expression tagged with its 'Type'.
withType :: T.Expr t -> Type t -> TypeChecker AnExpr
withType e ty = return (AnExpr e ty)


tyComponent :: Type Component
tyComponent = TyComponent Nothing


-- | @typeError expected actual region@ constructs a 'Type.Error'.
typeError :: [AType] -> Type t -> Region -> Type.Error
typeError expected actual =
    Type.TypeError (fmap renderAType expected) (renderType actual)
  where
    renderAType (AType ty) = renderType ty


renderType :: Type r -> Text
renderType = Text.pack . show . pretty
