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
    , reduce
    , componentConsts
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import           Data.List          (genericLength)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set


import Rbsc.Config

import           Rbsc.Data.Action
import           Rbsc.Data.Array     (Array)
import qualified Rbsc.Data.Array     as Array
import           Rbsc.Data.Component
import           Rbsc.Data.Function  (function, functionType)
import           Rbsc.Data.Name
import           Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..), Region)

import Rbsc.Syntax.Operators
import Rbsc.Syntax.Quantification
import Rbsc.Syntax.Typed.Expr     (Constants, HasConstants (..))
import Rbsc.Syntax.Typed.Expr     as T


type MonadEval r m
     = ( MonadError Error m
       , MonadReader r m
       , HasRecursionDepth r
       , HasConstants r
       )


data ReducerInfo = ReducerInfo
    { _riConstants    :: Constants
    , _remainingDepth :: !RecursionDepth
    , _region         :: !Region
    }

makeLenses ''ReducerInfo


-- | Evaluate an expression under a given set of constants.
eval :: MonadEval r m => Loc (Expr t) -> m t
eval e = do
    depth <- view recursionDepth
    cs    <- view constants
    case evalInternal cs depth e of
        Left err -> throwError err
        Right x  -> return x


-- | Reduce an expression as far as possible by evaluating constant
-- sub-expressions.
reduce :: MonadEval r m => Loc (Expr t) -> m (Loc (Expr t))
reduce e = do
    depth <- view recursionDepth
    cs    <- view constants
    case reduceInternal cs depth e of
        Left err -> throwError err
        Right e' -> return e'


type Reducer a = ReaderT ReducerInfo (Either Error) a

runReducer ::
       Reducer a -> Constants -> RecursionDepth -> Region -> Either Error a
runReducer m cs depth rgn = runReaderT m (ReducerInfo cs depth rgn)


evalInternal :: Constants -> RecursionDepth -> Loc (Expr t) -> Either Error t
evalInternal cs depth e = do
    Loc e' _ <- reduceInternal cs depth e
    case e' of
        Literal x _ -> return x
        _           -> throw (getLoc e) NotConstant


reduceInternal ::
       Constants
    -> RecursionDepth
    -> Loc (Expr t)
    -> Either Error (Loc (Expr t))
reduceInternal cs depth (Loc e rgn) =
    Loc <$> runReducer (go e) cs depth rgn <*> pure rgn
  where
    go :: Expr t -> Reducer (Expr t)
    go e' = case e' of
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

        Quantified q (QdTypeComponent tySet) sc -> do
            comps <- componentConsts tySet <$> view riConstants
            go (quantifier q (fmap (instantiate sc) comps))

        Quantified q (QdTypeInt (Loc lower rgnL, Loc upper rgnU)) sc -> do
            lower' <- go lower
            upper' <- go upper
            case (lower', upper') of
                (Literal l _, Literal u _) -> do
                    let es = fmap
                                (\i -> SomeExpr (Literal i TyInt) TyInt)
                                [l .. u]
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
    Identifier name ty ->
        view (riConstants.at name) >>= \case
            Just (SomeExpr e' ty') -> case typeEq ty ty' of -- if the identifier is a constant ...
                Just Refl -> return e' -- ... then replace by constant value
                Nothing   -> error "toLiteral: type error"
            Nothing -> case ty of
                TyAction -> return (Literal (Action name) TyAction)
                _        -> return e

    LitArray es -> return $ case toArray es of
        Just (arr, ty) -> Literal arr ty
        Nothing        -> e

    Cast (Literal x _) ->
        return (Literal (fromIntegral x) TyDouble)

    Not (Literal x _) ->
        return (Literal (not x) TyBool)

    Negate (Literal x ty) ->
        return (Literal (negate x) ty)

    ArithOp aOp (Literal l ty) (Literal r _) ->
        return (Literal (arithOp aOp l r) ty)

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

    Member (Literal comp _) name TyAction ->
        return (Literal (LocalAction (view compName comp) name) TyAction)

    Index (Literal arr (TyArray _ innerTy)) (LitIndex i rgn) ->
        case Array.index arr i of
            Just x  -> return (Literal x innerTy)
            Nothing -> throw rgn (IndexOutOfBounds (Array.bounds arr) i)

    Index (LitArray arr) (LitIndex i rgn)
        | i >= NonEmpty.length arr ->
            throw rgn (IndexOutOfBounds (0, NonEmpty.length arr - 1) i)
        | otherwise -> return (arr NonEmpty.!! i)

    Index (ActionArray (Literal act _)) (LitIndex i _) ->
        return (Literal (IndexedAction act i) TyAction)

    HasType (Literal comp _) tyName ->
        return (Literal (view compTypeName comp == tyName) TyBool)

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

    _ -> return e


pattern LitIndex :: (Num a, Integral t, Show t) => a -> Region -> Loc (Expr t)
pattern LitIndex i rgn <- Loc (Literal (fromIntegral -> i) _) rgn


-- | Transforms an array into an array value if the array only consists of
-- literals.
toArray :: NonEmpty (Expr t) -> Maybe (Array t, Type (Array t))
toArray xs@(lit :| xs') = case lit of
    Literal _ ty -> (,)
        <$> fmap Array.fromList (traverse f (NonEmpty.toList xs))
        <*> pure (TyArray (0, length xs') ty)
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
componentConsts tySet = mapMaybe f . Map.elems
  where
    f = \case
        e@(SomeExpr _ (TyComponent ty))
            | ty `Set.isSubsetOf` tySet -> Just e
        _ -> Nothing


isElement :: Component -> SomeExpr -> Bool
isElement comp = \case
    SomeExpr (Literal role _) (TyComponent _) ->
        view compContainedIn role == Just (view compName comp)
    _ -> False
