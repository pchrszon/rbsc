{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Evaluation of typed expressions.
module Rbsc.Eval
    ( MonadEval
    , eval
    , evalConstDef
    , reduce
    , componentConsts
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import           Data.Foldable
import           Data.List          (genericLength)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict    as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set


import Rbsc.Config

import           Rbsc.Data.Action
import           Rbsc.Data.Array     (Array)
import qualified Rbsc.Data.Array     as Array
import           Rbsc.Data.Component
import           Rbsc.Data.Field
import           Rbsc.Data.Function  (function, functionType)
import           Rbsc.Data.Name
import           Rbsc.Data.Scope
import           Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..), Region)

import Rbsc.Syntax.Operators
import Rbsc.Syntax.Quantification
import Rbsc.Syntax.Typed.Expr     (Constants, constants)
import Rbsc.Syntax.Typed.Expr     as T


type MonadEval r m
     = ( MonadError Error m
       , MonadReader r m
       , Has RecursionDepth r
       , Has Constants r
       , Has Methods r
       )


data ReducerInfo = ReducerInfo
    { _riConstants    :: Constants
    , _riMethods      :: Methods
    , _remainingDepth :: !RecursionDepth
    , _region         :: !Region
    }

makeLenses ''ReducerInfo


-- | Evaluate an expression under a given set of constants.
eval :: MonadEval r m => Loc (Expr t) -> m t
eval = eval' False


-- | Evaluate a constant definition under a given set of constants.
evalConstDef :: MonadEval r m => Loc (Expr t) -> m t
evalConstDef = eval' True


eval' :: MonadEval r m => Bool -> Loc (Expr t) -> m t
eval' inConst e = do
    info <- getEvalInfo
    case evalInternal info inConst e of
        Left err -> throwError err
        Right x  -> return x


-- | Reduce an expression as far as possible by evaluating constant
-- sub-expressions.
reduce :: MonadEval r m => Loc (Expr t) -> m (Loc (Expr t))
reduce e = do
    info <- getEvalInfo
    case reduceInternal info False e of
        Left err -> throwError err
        Right e' -> return e'


type Reducer a = ReaderT ReducerInfo (Either Error) a

runReducer
    :: Reducer a
    -> EvalInfo
    -> Region
    -> Either Error a
runReducer m (EvalInfo depth cs ms) rgn =
    runReaderT m (ReducerInfo cs ms depth rgn)


data EvalInfo = EvalInfo !RecursionDepth !Constants !Methods


getEvalInfo :: MonadEval r m => m EvalInfo
getEvalInfo = EvalInfo
    <$> view recursionDepth
    <*> view constants
    <*> view methods


evalInternal
    :: EvalInfo
    -> Bool
    -> Loc (Expr t)
    -> Either Error t
evalInternal info inConst e = do
    Loc e' _ <- reduceInternal info inConst e
    case e' of
        Literal x _ -> return x
        _           -> throw (getLoc e) NotConstant


reduceInternal
    :: EvalInfo
    -> Bool
    -> Loc (Expr t)
    -> Either Error (Loc (Expr t))
reduceInternal info inConst (Loc e rgn) =
    Loc <$> runReducer (go e) info rgn <*> pure rgn
  where
    go :: Expr t -> Reducer (Expr t)
    go e' = case e' of
        Identifier name ty ->
            view (riConstants.at name) >>= \case
                Just (SomeExpr e'' ty') -> case typeEq ty ty' of -- if the identifier is a constant ...
                    Just Refl -> go e'' -- ... then replace by constant value
                    Nothing   -> error "toLiteral: type error"
                Nothing -> case ty of
                    TyAction -> return (Literal (Action name) TyAction)
                    _        -> return e'

        LogicOp lOp l r -> do
            l' <- go l
            case l' of
                Literal x _ -> case logicOpShortcut lOp x of
                    Just b -> return (Literal b TyBool)
                    Nothing -> do
                        r' <- go r
                        toLiteral (LogicOp lOp l' r')
                _ -> do
                    r' <- go r
                    toLiteral (LogicOp lOp l' r')

        Member c name ty -> do
            c' <- go c
            case c' of
                Literal comp (TyComponent (toList -> [tyName])) ->
                    view (riMethods.at (ScopedName (Local tyName) name)) >>= \case
                        Just (SomeExpr e'' ty') -> case typeEq ty ty' of
                            Just Refl -> go (substituteSelf comp e'')
                            Nothing   -> error "toLiteral: type error"
                        Nothing -> toLiteral (Member c' name ty)
                _ -> toLiteral (Member c' name ty)

        Apply f arg -> do
            f' <- go f
            case f' of
                Lambda ty body -> do
                    checkDepth
                    let body' = instantiate body (SomeExpr arg ty)
                    local (remainingDepth %~ subtract 1) $
                        go body'
                _ -> do
                    arg' <- go arg
                    return (tryEvalBuiltIn f' arg')

        IfThenElse cond _then _else -> do
            cond' <- go cond
            case cond' of
                Literal True _  -> go _then
                Literal False _ -> go _else
                _ -> do
                    _then' <- go _then
                    _else' <- go _else
                    return (IfThenElse cond' _then' _else')

        -- Do not reduce under lambda, because binders should be removed
        -- top-down.
        Lambda _ _ -> return e'

        GenArray gen l u -> do
            -- Non-emptiness of [l .. u] is checked in 'tcExpr'.
            let es = NonEmpty.fromList (fmap toIntExpr [l .. u])
            go (LitArray (fmap (instantiate (Scoped gen)) es))

        -- When evaluating a constant definition, the set of components is
        -- not fixed yet, so any quantification over components is not
        -- constant.
        Quantified q (QdTypeComponent tySet) sc | not inConst -> do
            comps <- componentConsts tySet <$> view riConstants
            go (quantifier q (fmap (instantiate sc) comps))

        Quantified q (QdTypeInt (Loc lower rgnL, Loc upper rgnU)) sc -> do
            lower' <- go lower
            upper' <- go upper
            case (lower', upper') of
                (Literal l _, Literal u _) -> do
                    let es = fmap toIntExpr [l .. u]
                    go (quantifier q (fmap (instantiate sc) es))
                _ -> return (Quantified q
                        (QdTypeInt (Loc lower' rgnL, Loc upper' rgnU)) sc)

        _ -> plateExpr go e' >>= toLiteral


-- | A built-in function can be evaluated if all arguments are provided
-- (i.e. it is not partially applied) and all arguments are 'Literal's.
tryEvalBuiltIn :: Show b => Expr (Fn (a -> b)) -> Expr a -> Expr b
tryEvalBuiltIn f arg =
    let e = Apply f arg
    in case go e of
        Literal _ (TyFunc _ _) -> e -- the function is partially applied
        l@(Literal _ _)        -> l -- the function application was fully evaluated
        _                      -> e
  where
    go :: Expr t -> Expr t
    go e = case e of
        LitFunction func ->
            Literal (Fn (function func)) (functionType func)
        Apply f' (Literal argLit _) ->
            case go f' of
                Literal (Fn funcLit) (TyFunc _ ty) ->
                    Literal (funcLit argLit) ty
                _ -> e
        _ -> e


-- | Reduces an expression to a literal if possible, otherwise the original
-- expression is returned. An expression can only be reduced if all
-- sub-expressions are literals.
toLiteral :: Expr t -> Reducer (Expr t)
toLiteral e = case e of
    LitArray es -> return $ case toArray es of
        Just (arr, ty) -> Literal arr ty
        Nothing        -> e

    Cast (Literal x _) ->
        return (Literal (fromIntegral x) TyDouble)

    Not (Literal x _) ->
        return (Literal (not x) TyBool)

    Negate (Literal x ty) ->
        return (Literal (negate x) ty)

    ArithOp aOp l (Literal 0 _) | aOp == Add || aOp == Sub ->
        return l

    ArithOp Add (Literal 0 _) r ->
        return r

    ArithOp Mul l (Literal 1 _) ->
        return l

    ArithOp Mul (Literal 1 _) r ->
        return r

    ArithOp aOp (Literal l ty) (Literal r _) ->
        return (Literal (arithOp aOp l r) ty)

    Divide _ l (Literal 1 _) ->
        return l

    Divide rgn (Literal l _) (Literal r _)
        | r == 0.0  -> throw rgn DivisionByZero
        | otherwise -> return (Literal (l / r) TyDouble)

    EqOp eOp _ (Literal l _) (Literal r _) ->
        return (Literal (eqOp eOp l r) TyBool)

    RelOp rOp (Literal l _) (Literal r _) ->
        return (Literal (relOp rOp l r) TyBool)

    LogicOp And (Literal True _) r ->
        return r

    LogicOp And l (Literal True _) ->
        return l

    -- the case "false and _" is already handled in "reduceInternal"
    LogicOp And _ (Literal False _) ->
        return (Literal False TyBool)

    LogicOp Or (Literal False _) r ->
        return r

    LogicOp Or l (Literal False _) ->
        return l

    -- the case "true or _" is already handled in "reduceInternal"
    LogicOp Or _ (Literal True _) ->
        return (Literal True TyBool)

    LogicOp lOp (Literal l _) (Literal r _) ->
        return (Literal (logicOp lOp l r) TyBool)

    LogicOp Implies (Literal True _) r ->
        return r

    Member (Literal comp _) name TyAction ->
        return (Literal (LocalAction (view compName comp) name) TyAction)

    Index (Literal arr (TyArray _ innerTy)) _ (LitIndex i rgn) ->
        case Array.index arr i of
            Just x  -> return (Literal x innerTy)
            Nothing -> throw rgn (IndexOutOfBounds (Array.size arr) i)

    Index (LitArray arr) _ (LitIndex i rgn)
        | i >= NonEmpty.length arr ->
            throw rgn (IndexOutOfBounds (NonEmpty.length arr) i)
        | otherwise -> return (arr NonEmpty.!! i)

    Index (ActionArray (Literal act _)) _ (LitIndex i _) ->
        return (Literal (IndexedAction act i) TyAction)

    HasType (Literal comp _) tySet ->
        return (Literal (view compTypeName comp `Set.member` tySet) TyBool)

    BoundTo (Loc (Literal role _) _) (Loc (Literal player _) _) ->
        return (Literal
            (view compBoundTo role == Just (view compName player)) TyBool)

    Element (Loc (Literal role _) _) (Loc (Literal compartment _) _) ->
        return (Literal
            (view compContainedIn role == Just (view compName compartment))
            TyBool)

    Count tySet (Literal comp _) -> do
        comps <- componentConsts tySet <$> view riConstants
        return (Literal (genericLength (filter (isElement comp) comps)) TyInt)

    Player (Loc (Literal comp _) rgn) ->
        case view compBoundTo comp of
            Just (ComponentName name (Just idx)) ->
                view (riConstants.at name) >>= \case
                    Just (SomeExpr (Literal arr _) (TyArray _ ty'@(TyComponent _))) ->
                        case Array.index arr idx of
                            Just comp' -> return (Literal comp' ty')
                            Nothing ->
                                error "toLiteral: component index out of bounds"
                    _ -> error $
                        "toLiteral: " ++ show name ++ " not in constant table"
            Just (ComponentName name Nothing) ->
                view (riConstants.at name) >>= \case
                    Just (SomeExpr (Literal comp' _) ty'@(TyComponent _)) ->
                        return (Literal comp' ty')
                    _ -> error $
                        "toLiteral: " ++ show name ++ " not in constant table"
            Nothing ->
                throw rgn (HasNoPlayer (componentName (view compName comp)))

    ComponentIndex (Loc (Literal comp _) rgn) ->
        case view compName comp of
            ComponentName _ (Just idx) -> return (Literal idx TyInt)
            ComponentName name Nothing ->
                throw rgn (NonIndexedComponent name)

    _ -> return e


pattern LitIndex :: (Num a, Integral t, Show t) => a -> Region -> Loc (Expr t)
pattern LitIndex i rgn <- Loc (Literal (fromIntegral -> i) _) rgn


-- | Transforms an array into an array value if the array only consists of
-- literals.
toArray :: NonEmpty (Expr t) -> Maybe (Array t, Type (Array t))
toArray xs@(lit :| _) = case lit of
    Literal _ ty -> (,)
        <$> fmap Array.fromList (traverse f (NonEmpty.toList xs))
        <*> pure (TyArray (NonEmpty.length xs) ty)
    _ -> Nothing
  where
    f (Literal x _) = Just x
    f _             = Nothing


-- | Check whether the maximum recursion depth has been reached. If so, an
-- 'ExceededDepth' error is thrown.
checkDepth :: Reducer ()
checkDepth = do
    depth <- view remainingDepth
    rgn   <- view region
    when (depth <= 0) (throw rgn ExceededDepth)


quantifier :: Quantifier t -> [Expr t] -> Expr t
quantifier q = foldr qOp neutralElement
  where
    qOp = case q of
        Forall  -> LogicOp And
        Exists  -> LogicOp Or
        Sum     -> ArithOp Add
        Product -> ArithOp Mul

    neutralElement = case q of
        Forall  -> Literal True TyBool
        Exists  -> Literal False TyBool
        Sum     -> Literal 0 TyInt
        Product -> Literal 1 TyInt


-- | Get a list of all constants that have a component type contained in
-- the given set.
componentConsts :: Set TypeName -> Constants -> [SomeExpr]
componentConsts tySet = concatMap f . Map.elems
  where
    f :: SomeExpr -> [SomeExpr]
    f = \case
        e@(SomeExpr _ (TyComponent ty))
            | ty `Set.isSubsetOf` tySet -> [e]
        SomeExpr (Literal arr (TyArray _ (TyComponent ty))) _
            | ty `Set.isSubsetOf` tySet ->
               fmap (toSomeExpr (TyComponent ty)) (toList arr)
        _ -> []

    toSomeExpr ty comp = SomeExpr (Literal comp ty) ty


substituteSelf :: Component -> Expr t -> Expr t
substituteSelf comp = transformExpr subst
  where
    subst :: Expr t -> Expr t
    subst e = case e of
        Self -> Literal comp
            (TyComponent (Set.singleton (view compTypeName comp)))
        _ -> e


isElement :: Component -> SomeExpr -> Bool
isElement comp = \case
    SomeExpr (Literal role _) (TyComponent _) ->
        view compContainedIn role == Just (view compName comp)
    _ -> False


toIntExpr :: Int -> SomeExpr
toIntExpr i = SomeExpr (Literal i TyInt) TyInt
