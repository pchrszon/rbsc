{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}


-- | Type checking of expressions.
module Rbsc.TypeChecker.Expr
    ( tcExpr
    , tcAction
    , tcRoleConstraint
    , tcFunctionDef
    , tcQuantifiedType
    , hasType
    ) where


import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import           Data.Foldable
import           Data.Function
import           Data.List          (nubBy)
import           Data.List.NonEmpty (NonEmpty (..), fromList)
import qualified Data.Map.Strict    as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Traversable


import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.Function
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..), Region, withLocOf)
import Rbsc.Report.Result

import           Rbsc.Syntax.Quantification
import           Rbsc.Syntax.Typed.Expr     (SomeExpr (..))
import qualified Rbsc.Syntax.Typed.Expr     as T
import qualified Rbsc.Syntax.Untyped.Expr   as U

import Rbsc.TypeChecker.Internal

import Rbsc.Util (renderPretty)


-- | Type check an action.
tcAction :: Loc U.Expr -> TypeChecker SomeExpr
tcAction e = local (set context ActionContext) $ do
    e' <- e `hasType` TyAction
    e' `withType` TyAction


-- | Type check a role constraint. The returned expression will have
-- 'TyBool'.
tcRoleConstraint :: Loc U.Expr -> TypeChecker SomeExpr
tcRoleConstraint e = local (set context ConstraintContext) $ do
    e' <- e `hasType` TyBool
    e' `withType` TyBool


-- | Type check an untyped expression. If the expression is well-typed,
-- a typed expression is returned, together with a type-level witness.
tcExpr :: Loc U.Expr -> TypeChecker SomeExpr
tcExpr (Loc e rgn) = case e of
    U.LitBool b ->
        T.Literal b TyBool `withType` TyBool

    U.LitInt i ->
        T.Literal i TyInt `withType` TyInt

    U.LitDouble d ->
        T.Literal d TyDouble `withType` TyDouble

    U.LitAction inner ->
        local (set context ActionContext) $ do
            inner' <- inner `hasType` TyAction
            inner' `withType` TyAction

    U.LitFunction f ->
        return (fromFunctionName f)

    U.LitArray (inner :| inners) -> do
        SomeExpr inner' ty <- tcExpr inner
        Dict <- pure (dictShow ty)
        inners' <- traverse (`hasType` ty) inners
        let arrayTy = TyArray (length inners + 1) ty
        return (SomeExpr (T.LitArray (inner' :| inners')) arrayTy)

    U.GenArray inner var lower upper -> do
        SomeExpr inner' ty <- local (over boundVars ((var, Some TyInt) :)) $
            tcExpr inner
        Dict <- pure (dictShow ty)

        lower' <- lower `hasType` TyInt
        upper' <- upper `hasType` TyInt
        l <- eval (lower' `withLocOf` lower)
        u <- eval (upper' `withLocOf` upper)

        when (u < l) (throw rgn (EmptyGenArray l u))

        return (SomeExpr (T.GenArray inner' l u) (TyArray (u - l + 1) ty))

    U.Constraint inner ->
        local (set context ConstraintContext) $ do
            inner' <- inner `hasType` TyBool
            inner' `withType` TyBool

    U.Self ->
        view scope >>= \case
            Global       -> throw rgn SelfOutsideImpl
            Local tyName -> T.Self `withType` TyComponent (Set.singleton tyName)

    U.Identifier name ->
        lookupBoundVar name >>= \case
            Just (i, Some ty) ->
                T.Bound i ty `withType` ty
            Nothing -> do
                Some ty <- getIdentifierType name rgn
                T.Identifier name ty `withType` ty

    U.Not inner -> do
        inner' <- inner `hasType` TyBool
        T.Not inner' `withType` TyBool

    U.Negate inner -> do
        SomeExpr inner' ty <- tcExpr inner
        Dict <- isNumType ty (getLoc inner)
        T.Negate inner' `withType` ty

    U.ArithOp aOp l r -> do
        (SomeExpr l' tyL, SomeExpr r' tyR) <-
            binaryCast <$> tcExpr l <*> tcExpr r
        Dict <- isNumType tyL (getLoc l)
        _    <- isNumType tyR (getLoc r)
        Dict <- isEqType tyL (getLoc l)
        Refl <- expect tyL (getLoc r) tyR
        T.ArithOp aOp l' r' `withType` tyL

    U.Divide l r -> do
        l' <- l `hasType` TyDouble
        r' <- r `hasType` TyDouble
        T.Divide rgn l' r' `withType` TyDouble

    U.EqOp eOp l r -> do
        (SomeExpr l' tyL, SomeExpr r' tyR) <-
            binaryCast <$> tcExpr l <*> tcExpr r
        Dict <- isEqType tyL (getLoc l)
        _    <- isEqType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.EqOp eOp tyL l' r' `withType` TyBool

    U.RelOp rOp l r -> do
        (SomeExpr l' tyL, SomeExpr r' tyR) <-
            binaryCast <$> tcExpr l <*> tcExpr r
        Dict <- isOrdType tyL (getLoc l)
        _    <- isOrdType tyR (getLoc r)
        Refl <- expect tyL (getLoc r) tyR
        T.RelOp rOp l' r' `withType` TyBool

    U.LogicOp lOp l r -> do
        l' <- l `hasType` TyBool
        r' <- r `hasType` TyBool
        T.LogicOp lOp l' r' `withType` TyBool

    U.Member inner name -> do
        tyComponent <- getTyComponent
        (inner', ty) <- inner `hasType'` tyComponent
        case ty of
            TyComponent tySet -> do
                memberTys <- getLocalVarTypes name tySet
                Some memberTy <- getMemberType rgn name memberTys
                T.Member inner' name memberTy `withType` memberTy

    U.Index inner idx -> do
        SomeExpr inner' ty <- tcExpr inner
        idx' <- idx `hasType` TyInt
        case ty of
            TyArray s elemTy -> do
                Dict <- return (dictShow elemTy)
                T.Index inner' (Just s) (idx' `withLocOf` idx) `withType` elemTy
            TyAction ->
                T.Index (T.ActionArray inner') Nothing (idx' `withLocOf` idx)
                `withType` TyAction
            _ -> throw (getLoc inner) (NotAnArray (renderPretty ty))

    U.Call f args -> do
        f' <- tcExpr f
        checkCallArity rgn f' args
        tcCall (Loc f' (getLoc f)) args

    U.IfThenElse cond _then _else -> do
        cond' <- cond `hasType` TyBool
        (SomeExpr _then' tyT, SomeExpr _else' tyE) <-
            binaryCast <$> tcExpr _then <*> tcExpr _else
        Refl <- expect tyT (getLoc _else) tyE

        -- The branches of the if expression may return different component
        -- types. Therefore, we generate the union of those types as result
        -- type of the if expression.
        let ty = typeUnion tyT tyE
        T.IfThenElse cond' _then' _else' `withType` ty

    U.HasType inner tyName ->
        whenTypeExists tyName $ do
            tyComponent <- getTyComponent
            inner' <- inner `hasType` tyComponent
            T.HasType inner' (unLoc tyName) `withType` TyBool

    U.BoundTo l r -> do
        tyComponent <- getTyComponent
        (l', tyL) <- l `hasType'` tyComponent
        case tyL of
            TyComponent tySet ->
                warnIfNot _RoleType tySet (NonRoleInRelation rgn)

        r' <- r `hasType` tyComponent
        T.BoundTo (l' `withLocOf` l) (r' `withLocOf` r) `withType` TyBool

    U.Element l r -> do
        tyComponent <- getTyComponent
        (l', tyL) <- l `hasType'` tyComponent
        case tyL of
            TyComponent tySetL ->
                warnIfNot _RoleType tySetL (NonRoleInRelation rgn)

        (r', tyR) <- r `hasType'` tyComponent
        case tyR of
            TyComponent tySetR ->
                warnIfNot _CompartmentType tySetR (NonCompartmentInRelation rgn)

        T.Element (l' `withLocOf` l) (r' `withLocOf` r) `withType` TyBool

    U.Count tySet inner -> do
        tyComponent <- getTyComponent
        tySet' <- normalizeTypeSet' tySet
        inner' <- inner `hasType` tyComponent
        T.Count tySet' inner' `withType` TyInt

    U.Length inner -> do
        SomeExpr _ ty <- tcExpr inner
        case ty of
            TyArray size _ ->
                T.Literal size TyInt `withType` TyInt
            _ -> throw (getLoc inner) (NotAnArray (renderPretty ty))

    U.HasPlayer inner -> do
        tyComponent <- getTyComponent
        inner' <- inner `hasType` tyComponent
        T.HasPlayer inner' `withType` TyBool

    U.Player inner -> do
        tyComponent <- getTyComponent
        compTys <- view componentTypes
        (inner', ty) <- inner `hasType'` tyComponent
        let tySet' = case ty of
                TyComponent tySet ->
                    Set.unions . flip fmap (toList tySet) $ \tyName ->
                        case Map.lookup tyName compTys of
                            Just (RoleType playerTyNames) -> playerTyNames
                            _                             -> Set.empty
        when (Set.null tySet') (throw rgn NoPossiblePlayers)
        T.Player (inner' `withLocOf` inner) `withType` TyComponent tySet'

    U.Playable inner mAct -> do
        tyComponent <- getTyComponent
        (inner', ty) <- inner `hasType'` tyComponent
        case ty of
            TyComponent tySet -> checkIfRole (getLoc inner) tySet

        mAct' <- case mAct of
            Just act -> local (set context ActionContext) $ do
                act' <- act `hasType` TyAction
                return (Just (act' `withLocOf` act))
            Nothing -> return Nothing

        T.Playable (inner' `withLocOf` inner) mAct' `withType` TyBool


    U.ComponentIndex inner -> do
        tyComponent <- getTyComponent
        inner' <- inner `hasType` tyComponent
        T.ComponentIndex (inner' `withLocOf` inner) `withType` TyInt

    U.Quantified q var qdTy body -> do
        (qdTy', varTy) <- tcQuantifiedType qdTy
        Some q' <- return (typedQuantifier q)
        let qTy = quantifierType q'
        body' <- local (over boundVars ((var, varTy) :)) $
            body `hasType` qTy
        T.Quantified q' qdTy' (T.Scoped body') `withType` qTy


-- | @tcFunctionDef params tyRes body@ checks an untyped function
-- definition. The parameter list @params@ is transformed into a sequence
-- of lambda abstractions.
tcFunctionDef ::
       [(Name, Some Type)] -> Some Type -> Loc U.Expr -> TypeChecker SomeExpr
tcFunctionDef params (Some tyRes) body =
    -- params must be reversed because the first parameter corresponds
    -- to the outermost lambda and thus has the highest De-Bruijn index.
    local (over boundVars (reverse params ++)) $ do
        body' <- body `hasType` tyRes
        return (foldr mkLambda (SomeExpr body' tyRes) params)
  where
    mkLambda (_, Some ty) (SomeExpr e tyRes') =
        SomeExpr (T.Lambda ty (T.Scoped e)) (ty --> tyRes')


fromFunctionName :: FunctionName -> SomeExpr
fromFunctionName = \case
    FuncMinInt ->
        SomeExpr (T.LitFunction MinInt) (TyInt --> TyInt --> TyInt)
    FuncMinDouble ->
        SomeExpr (T.LitFunction MinDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMaxInt ->
        SomeExpr (T.LitFunction MaxInt) (TyInt --> TyInt --> TyInt)
    FuncMaxDouble ->
        SomeExpr (T.LitFunction MaxDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncFloor ->
        SomeExpr (T.LitFunction Floor) (TyDouble --> TyInt)
    FuncCeil ->
        SomeExpr (T.LitFunction Ceil) (TyDouble --> TyInt)
    FuncPowInt ->
        SomeExpr (T.LitFunction PowInt) (TyInt --> TyInt --> TyInt)
    FuncPowDouble ->
        SomeExpr (T.LitFunction PowDouble) (TyDouble --> TyDouble --> TyDouble)
    FuncMod ->
        SomeExpr (T.LitFunction Mod) (TyInt --> TyInt --> TyInt)
    FuncLog ->
        SomeExpr (T.LitFunction Log) (TyDouble --> TyDouble --> TyDouble)


checkCallArity :: Region -> SomeExpr -> [args] -> TypeChecker ()
checkCallArity rgn (SomeExpr _ ty) args
    | numParams > 0 && numParams < numArgs =
        throw rgn (WrongNumberOfArguments numParams numArgs)
    | otherwise = return ()
  where
    numParams = paramsLength ty
    numArgs   = length args

    paramsLength :: Type t -> Int
    paramsLength = \case
        TyFunc _ tyRes -> 1 + paramsLength tyRes
        _              -> 0


tcCall :: Loc SomeExpr -> [Loc U.Expr] -> TypeChecker SomeExpr
tcCall (Loc (SomeExpr e ty) _) [] = return (SomeExpr e ty)
tcCall (Loc (SomeExpr f (TyFunc tyParam tyRes)) rgn) (arg : args) = do
    arg' <- arg `hasType` tyParam
    Dict <- return (dictShow tyRes)
    tcCall (Loc (SomeExpr (T.Apply f arg') tyRes) rgn) args
tcCall (Loc (SomeExpr _ ty) rgn) (_ : _) =
    throw rgn (NotAFunction (renderPretty ty))


-- | @getLocalVarTypes name tySet@ returns the type of the member @name@
-- for each type in @tySet@. If a type does not have a member of the given
-- @name@, then @Nothing@ is returned for that type.
getLocalVarTypes ::
       Name -> Set TypeName -> TypeChecker [(TypeName, Maybe (Some Type))]
getLocalVarTypes name tySet = for (Set.toList tySet) $ \tyName -> do
    ctx <- view context
    mTy <- view (symbolTable.at (ScopedName (Local tyName) name))

    -- If we are inside action brackets, all undefined members are actions.
    let mAct = case ctx of
            ActionContext -> Just (Some TyAction)
            _             -> Nothing

    return (tyName, mTy <|> mAct)


-- | @getMemberType@ checks whether all members are defined and their types
-- agree. If so, the single common member type is returned.
getMemberType ::
       Region
    -> Name
    -> [(TypeName, Maybe (Some Type))]
    -> TypeChecker (Some Type)
getMemberType rgn name memberTys
    | not (null undefineds) = throw rgn (UndefinedMember undefineds name)
    | otherwise = case nubBy ((==) `on` snd) defineds of
        [] -> error "getMemberType: empty list"
        [(_, ty)] -> return ty
        ((firstTyName, Some firstTy):(secondTyName, Some secondTy):_) ->
            throw rgn
                (ConflictingMemberTypes
                     name
                     firstTyName
                     (renderPretty firstTy)
                     secondTyName
                     (renderPretty secondTy))
  where
    (defineds, undefineds) = foldr f ([], []) memberTys
    f memberTy (ds, us) = case memberTy of
        (tyName, Just ty) -> ((tyName, ty) : ds, us)
        (tyName, Nothing) -> (ds, tyName : us)


tcQuantifiedType ::
       QuantifiedType ComponentTypeSet (Loc U.Expr)
    -> TypeChecker (T.TQuantifiedType, Some Type)
tcQuantifiedType (QdTypeComponent tySet) = do
    tySet' <- normalizeTypeSet' tySet
    return (QdTypeComponent tySet', Some (TyComponent tySet'))
tcQuantifiedType (QdTypeInt (lower, upper)) = do
    lower' <- lower `hasType` TyInt
    upper' <- upper `hasType` TyInt
    let qdTy = QdTypeInt (lower' `withLocOf` lower, upper' `withLocOf` upper)
    return (qdTy, Some TyInt)


typedQuantifier :: U.Quantifier -> Some T.Quantifier
typedQuantifier = \case
    U.Forall  -> Some T.Forall
    U.Exists  -> Some T.Exists
    U.Sum     -> Some T.Sum
    U.Product -> Some T.Product


quantifierType :: T.Quantifier t -> Type t
quantifierType = \case
    T.Forall  -> TyBool
    T.Exists  -> TyBool
    T.Sum     -> TyInt
    T.Product -> TyInt


normalizeTypeSet' :: ComponentTypeSet -> TypeChecker (Set TypeName)
normalizeTypeSet' tySet = do
    compTys <- view componentTypes
    tySetDefs <- view typeSets
    lift (fromEither' (normalizeTypeSet compTys tySetDefs tySet))


-- | Assume that a given untyped expression has a given 'Type'.
--
-- If the expression has not the given type, but a dynamic cast is
-- possible, then the dynamic cast will be performed.
hasType :: Loc U.Expr -> Type t -> TypeChecker (T.Expr t)
hasType e expected = fst <$> hasType' e expected


hasType' :: Loc U.Expr -> Type t -> TypeChecker (T.Expr t, Type t)
hasType' e expected = do
    SomeExpr e' actual <- cast (getLoc e) expected =<< tcExpr e
    Refl <- expect expected (getLoc e) actual
    checkComponentType expected (getLoc e) actual
    return (e', actual)


-- | If one of the two given expression has 'TyDouble' and the other one
-- has 'TyInt', then cast the @TyInt@ expression to @TyDouble@.
binaryCast :: SomeExpr -> SomeExpr -> (SomeExpr, SomeExpr)
binaryCast (SomeExpr l TyDouble) (SomeExpr r TyInt) =
    (SomeExpr l TyDouble, SomeExpr (T.Cast r) TyDouble)
binaryCast (SomeExpr l TyInt) (SomeExpr r TyDouble) =
    (SomeExpr (T.Cast l) TyDouble, SomeExpr r TyDouble)
binaryCast l r = (l, r)


-- | @cast expected e@ inserts 'Cast's if a dynamic cast to type @expected@
-- is possible. Otherwise, the original expression is returned.
cast :: Region -> Type t -> SomeExpr -> TypeChecker SomeExpr
cast _ TyDouble (SomeExpr e TyInt) = T.Cast e `withType` TyDouble
cast _ (TyArray tSize TyDouble) (SomeExpr (T.LitArray es) (TyArray vSize TyInt))
    | tSize == vSize =
        T.LitArray (fmap T.Cast es) `withType` TyArray vSize TyDouble
cast _ arrTy@(TyArray tSize ty) e@(SomeExpr e' elemTy) =
    case typeEq ty elemTy of
        Just Refl -> case dictShow ty of
            Dict ->
                T.LitArray (fromList (replicate tSize e'))
                `withType` arrTy
        Nothing -> return e
cast rgn TyBool e@(SomeExpr e' (TyComponent tySet)) = do
    ctx <- view context
    case ctx of
        ConstraintContext -> do
            checkIfRole rgn tySet
            T.IsPlayed (Loc e' rgn) `withType` TyBool
        _ -> return e
cast _ _ e = return e


-- | If @expected@ and @actual@ are 'TyComponent',
-- @checkComponentType expected rgn actual@ checks if @actual@ is equal to
-- @expected@ or a subtype of @expected@.
checkComponentType :: Type t -> Region -> Type t -> TypeChecker ()
checkComponentType expected@(TyComponent tySetExp) rgn actual@(TyComponent tySetAct)
    | tySetAct `Set.isSubsetOf` tySetExp = return ()
    | otherwise = throw rgn (typeError [Some expected] actual)
checkComponentType _ _ _ = return ()


-- | If @l@ and @r@ are 'TyComponent', then @typeUnion l r@ returns the
-- component type subsuming both @l@ and @r@. Otherwise, the type @l@ is
-- returned unmodified.
typeUnion :: Type t -> Type t -> Type t
typeUnion (TyComponent tySetL) (TyComponent tySetR) =
    TyComponent (tySetL `Set.union` tySetR)
typeUnion l _ = l


-- | Get the most general 'Type' of components. This type subsumes all user
-- defined component types.
getTyComponent :: TypeChecker (Type Component)
getTyComponent = TyComponent <$> view (componentTypes.to Map.keysSet)


-- | Check whether each of the given component types is a role type. If
-- not, a 'CannotBePlayed' error is thrown.
checkIfRole :: Region -> Set TypeName -> TypeChecker ()
checkIfRole rgn tySet = do
    compTys <- view componentTypes
    for_ (toList tySet) $ \tyName ->
        unless (isRoleType compTys tyName) $
            throw rgn (CannotBePlayed (renderPretty (TyComponent tySet)) tyName)


warnIfNot :: Prism' ComponentType a -> Set TypeName -> Warning -> TypeChecker ()
warnIfNot p tySet w = do
    compTys <- view componentTypes
    unless (any (\tyName -> has (at tyName._Just.p) compTys) tySet) $
        lift (warn w)
