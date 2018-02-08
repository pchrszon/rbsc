{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Evaluation of typed expressions.
module Rbsc.Eval
    ( RecursionDepth
    , eval
    , reduce
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (mapMaybe)


import           Rbsc.Data.Array     (Array)
import qualified Rbsc.Data.Array     as Array
import           Rbsc.Data.Component
import           Rbsc.Data.Function  (Fn (..), function)
import           Rbsc.Data.Name
import           Rbsc.Data.Type
import           Rbsc.Data.Value

import qualified Rbsc.Report.Error.Eval as Eval
import           Rbsc.Report.Region     (Loc (..), Region)

import Rbsc.Syntax.Expr.Typed as T
import Rbsc.Syntax.Operators


-- | The recursion depth.
type RecursionDepth = Int


data ReducerInfo = ReducerInfo
    { _constants      :: Constants
    , _remainingDepth :: !RecursionDepth
    , _region         :: !Region
    }

makeLenses ''ReducerInfo


type Reducer a = ReaderT ReducerInfo (Either Eval.Error) a

runReducer ::
       Reducer a -> Constants -> RecursionDepth -> Region -> Either Eval.Error a
runReducer m cs depth rgn = runReaderT m (ReducerInfo cs depth rgn)


-- | Evaluate an expression under a given set of constants.
eval :: Constants -> RecursionDepth -> Loc (Expr t) -> Either Eval.Error t
eval cs depth e = do
    e' <- reduce cs depth e
    case e' of
        Literal x -> return x
        _         -> throwError (Eval.NotConstant (getLoc e))


-- | Reduce an expression as far as possible by evaluating constant
-- sub-expressions.
reduce ::
       Constants -> RecursionDepth -> Loc (Expr t) -> Either Eval.Error (Expr t)
reduce cs depth (Loc e rgn) = runReducer (reduce' e) cs depth rgn


reduce' :: Expr t -> Reducer (Expr t)
reduce' e = case e of
    LogicOp lOp l r -> do
        l' <- reduce' l
        case l' of
            Literal x -> case logicOpShortcut lOp x of
                Just b -> return (Literal b)
                Nothing -> do
                    r' <- reduce' r
                    toLiteral (LogicOp lOp l' r')
            _ -> do
                r' <- reduce' r
                return (LogicOp lOp l' r')
    _ -> descend reduce' e >>= toLiteral


toLiteral :: Expr t -> Reducer (Expr t)
toLiteral e = case e of
    Variable name ty -> do
        value <- view (constants.at name)
        case value of
            Just (Value v ty') -> case typeEq ty ty' of -- if the variable is a constant ...
                Just Refl -> return (Literal v) -- ... then replace by constant value
                Nothing   -> error "toLiteral: type error"
            Nothing -> return e

    Array es -> return $ case toArray es of
        Just arr -> Literal arr
        Nothing  -> e

    Function func ->
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
        | r == 0.0  -> throwError (Eval.DivisionByZero rgn)
        | otherwise -> return (Literal (l / r))

    EqOp eOp (Literal l) (Literal r) ->
        return (Literal (eqOp eOp l r))

    RelOp rOp (Literal l) (Literal r) ->
        return (Literal (relOp rOp l r))

    LogicOp lOp (Literal l) (Literal r) ->
        return (Literal (logicOp lOp l r))

    Index (Literal arr) (Loc (Literal (fromIntegral -> i)) rgn) ->
        case Array.index arr i of
            Just x  -> return (Literal x)
            Nothing ->
                throwError (Eval.IndexOutOfBounds (Array.length arr) i rgn)

    Apply (Literal (Fn f)) (Literal arg) ->
        return (Literal (f arg))

    Apply (Lambda ty body) arg -> do
        checkDepth
        let body' = instantiate body (SomeExpr arg ty)
        local (remainingDepth %~ (-) 1) $
            reduce' body'

    HasType (Literal comp) tyName ->
        return (Literal (view compTypeName comp == tyName))

    BoundTo (Literal role) (Literal player) ->
        return (Literal (view compBoundTo role == Just (view compName player)))

    Element (Literal role) (Literal compartment) ->
        return (Literal
            (view compContainedIn role == Just (view compName compartment)))

    Quantified q mTyName sc -> do
        comps <- components mTyName <$> view constants
        let ty        = TyComponent mTyName
            compExprs = fmap (\c -> T.SomeExpr (Literal c) ty) comps

        -- instantiate quantified expression and reduce
        es <- traverse (T.transformM toLiteral . instantiate sc) compExprs

        -- remove all literals that do not influence the truth value
        let es' = filterLiterals q es

        -- connect instantiated expressions
        return (quantifier q es')

    _ -> return e


toArray :: NonEmpty (Expr t) -> Maybe (Array t)
toArray = fmap Array.fromList . traverse f . NonEmpty.toList
  where
    f (Literal x) = Just x
    f _           = Nothing


checkDepth :: Reducer ()
checkDepth = do
    depth <- view remainingDepth
    rgn   <- view region
    when (depth <= 0) $
        throwError (Eval.ExceededDepth rgn)


filterLiterals :: Quantifier -> [Expr Bool] -> [Expr Bool]
filterLiterals q = filter (not . isNeutralElement q)

quantifier :: Quantifier -> [Expr Bool] -> Expr Bool
quantifier q = \case
    [] -> neutralElement q
    es -> foldr1 (LogicOp lOp) es
  where
    lOp = case q of
        Forall -> And
        Exists -> Or


neutralElement :: Quantifier -> Expr Bool
neutralElement = \case
    Forall -> Literal True
    Exists -> Literal False


isNeutralElement :: Quantifier -> Expr Bool -> Bool
isNeutralElement Forall = \case
    Literal True -> True
    _            -> False
isNeutralElement Exists = \case
    Literal False -> True
    _             -> False


-- | Get the list of all constants of type 'Component'. If a 'TypeName' is
-- given, only the 'Component's of this type are returned.
components :: Maybe TypeName -> Constants -> [Component]
components mTyName = mapMaybe getComponent . Map.elems
  where
    getComponent :: Value -> Maybe Component
    getComponent = \case
        Value comp (TyComponent tyName)
            | matchType tyName -> Just comp
        _ -> Nothing

    matchType tyName =
        case mTyName of
            Just tyName' -> tyName == Just tyName'
            Nothing      -> True
