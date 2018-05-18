{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Evaluation of typed expressions.
module Rbsc.Eval
    ( RecursionDepth
    , eval
    , reduce
    , componentConsts
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import           Data.List          (genericLength)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (mapMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as Set


import           Rbsc.Data.Action
import           Rbsc.Data.Array     (Array)
import qualified Rbsc.Data.Array     as Array
import           Rbsc.Data.Component
import           Rbsc.Data.Function  (Fn (..), function)
import           Rbsc.Data.Name
import           Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..), Region)

import Rbsc.Syntax.Expr.Typed     (Constants)
import Rbsc.Syntax.Expr.Typed     as T
import Rbsc.Syntax.Operators
import Rbsc.Syntax.Quantification


-- | The recursion depth.
type RecursionDepth = Int


data ReducerInfo = ReducerInfo
    { _constants      :: Constants
    , _remainingDepth :: !RecursionDepth
    , _region         :: !Region
    }

makeLenses ''ReducerInfo


type Reducer a = ReaderT ReducerInfo (Either Error) a

runReducer ::
       Reducer a -> Constants -> RecursionDepth -> Region -> Either Error a
runReducer m cs depth rgn = runReaderT m (ReducerInfo cs depth rgn)


-- | Evaluate an expression under a given set of constants.
eval :: Constants -> RecursionDepth -> Loc (Expr t) -> Either Error t
eval cs depth e = do
    e' <- reduce cs depth e
    case e' of
        Literal x -> return x
        _         -> throw (getLoc e) NotConstant


-- | Reduce an expression as far as possible by evaluating constant
-- sub-expressions.
reduce :: Constants -> RecursionDepth -> Loc (Expr t) -> Either Error (Expr t)
reduce cs depth (Loc e rgn) = runReducer (go e) cs depth rgn
  where
    go :: Expr t -> Reducer (Expr t)
    go e' = case e' of
        LogicOp lOp l r -> do
            l' <- go l
            case l' of
                Literal x -> case logicOpShortcut lOp x of
                    Just b -> return (Literal b)
                    Nothing -> do
                        r' <- go r
                        toLiteral (LogicOp lOp l' r')
                _ -> do
                    r' <- go r
                    return (LogicOp lOp l' r')

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
                    toLiteral (Apply f' arg')

        IfThenElse cond _then _else -> do
            cond' <- go cond
            case cond' of
                Literal True  -> go _then
                Literal False -> go _else
                _ -> do
                    _then' <- go _then
                    _else' <- go _else
                    return (IfThenElse cond' _then' _else')

        -- Do not reduce under lambda, because binders should be removed
        -- top-down.
        Lambda _ _ -> return e'

        Quantified q (QdTypeComponent tySet) sc -> do
            comps <- componentConsts tySet <$> view constants
            go (quantifier q (fmap (instantiate sc) comps))

        Quantified q (QdTypeInt (Loc lower rgnL, Loc upper rgnU)) sc -> do
            lower' <- go lower
            upper' <- go upper
            case (lower', upper') of
                (Literal l, Literal u) -> do
                    let es = fmap (\i -> SomeExpr (Literal i) TyInt) [l .. u]
                    go (quantifier q (fmap (instantiate sc) es))
                _ -> return (Quantified q
                        (QdTypeInt (Loc lower' rgnL, Loc upper' rgnU)) sc)

        _ -> descend go e' >>= toLiteral


-- | Reduces an expression to a literal if possible, otherwise the original
-- expression is returned. An expression can only be reduced if all
-- sub-expressions are literals.
toLiteral :: Expr t -> Reducer (Expr t)
toLiteral e = case e of
    Identifier name TyAction ->
        return (Literal (Action name))

    Identifier name ty ->
        view (constants.at name) >>= \case
            Just (SomeExpr e' ty') -> case typeEq ty ty' of -- if the identifier is a constant ...
                Just Refl -> return e' -- ... then replace by constant value
                Nothing   -> error "toLiteral: type error"
            Nothing -> return e

    LitArray es -> return $ case toArray es of
        Just arr -> Literal arr
        Nothing  -> e

    LitFunction func ->
        return (Literal (Fn (function func)))

    Cast (Literal x) ->
        return (Literal (fromInteger x))

    Not (Literal x) ->
        return (Literal (not x))

    Negate (Literal x) ->
        return (Literal (negate x))

    ArithOp aOp (Literal l) (Literal r) ->
        return (Literal (arithOp aOp l r))

    Divide rgn (Literal l) (Literal r)
        | r == 0.0  -> throw rgn DivisionByZero
        | otherwise -> return (Literal (l / r))

    EqOp eOp (Literal l) (Literal r) ->
        return (Literal (eqOp eOp l r))

    RelOp rOp (Literal l) (Literal r) ->
        return (Literal (relOp rOp l r))

    LogicOp lOp (Literal l) (Literal r) ->
        return (Literal (logicOp lOp l r))

    Member (Literal comp) name TyAction ->
        return (Literal (LocalAction (view compName comp) name))

    Index (Literal arr) (LitIndex i rgn) ->
        case Array.index arr i of
            Just x  -> return (Literal x)
            Nothing -> throw rgn (IndexOutOfBounds (Array.bounds arr) i)

    Index (LitArray arr) (LitIndex i rgn)
        | i >= NonEmpty.length arr ->
            throw rgn (IndexOutOfBounds (0, NonEmpty.length arr - 1) i)
        | otherwise -> return (arr NonEmpty.!! i)

    Index (ActionArray (Literal act)) (LitIndex i _) ->
        return (Literal (IndexedAction act i))

    Apply (Literal (Fn f)) (Literal arg) ->
        return (Literal (f arg))

    HasType (Literal comp) tyName ->
        return (Literal (view compTypeName comp == tyName))

    BoundTo (Loc (Literal role) _) (Loc (Literal player) _) ->
        return (Literal (view compBoundTo role == Just (view compName player)))

    Element (Loc (Literal role) _) (Loc (Literal compartment) _) ->
        return (Literal
            (view compContainedIn role == Just (view compName compartment)))

    Count tySet (Literal comp) -> do
        comps <- componentConsts tySet <$> view constants
        return (Literal (genericLength (filter (isElement comp) comps)))

    _ -> return e


pattern LitIndex :: (Num a, Integral t, Show t) => a -> Region -> Loc (Expr t)
pattern LitIndex i rgn <- Loc (Literal (fromIntegral -> i)) rgn


-- | Transforms an array into an array value if the array only consists of
-- literals.
toArray :: NonEmpty (Expr t) -> Maybe (Array t)
toArray = fmap Array.fromList . traverse f . NonEmpty.toList
  where
    f (Literal x) = Just x
    f _           = Nothing


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
        Forall  -> Literal True
        Exists  -> Literal False
        Sum     -> Literal 0
        Product -> Literal 1


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
    SomeExpr (Literal role) (TyComponent _) ->
        view compContainedIn role == Just (view compName comp)
    _ -> False
