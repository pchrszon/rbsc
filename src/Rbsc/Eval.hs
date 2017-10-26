{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Evaluation of typed expressions.
module Rbsc.Eval
    ( eval
    , reduce
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

import qualified Rbsc.Report.Error.Eval as Eval
import           Rbsc.Report.Region     (Loc (..))

import Rbsc.Component
import Rbsc.Name
import Rbsc.Syntax.Expr.Typed as T
import Rbsc.Syntax.Operators
import Rbsc.Type
import Rbsc.Value


newtype ReducerInfo = ReducerInfo
    { _constants :: Constants
    }

makeLenses ''ReducerInfo


type Reducer a = ReaderT ReducerInfo (Either Eval.Error) a

runReducer :: Reducer a -> Constants -> Either Eval.Error a
runReducer m cs = runReaderT m (ReducerInfo cs)


-- | Evaluate an expression under a given set of constants.
eval :: Constants -> Loc (Expr t) -> Either Eval.Error t
eval cs (Loc e rgn) = do
    e' <- reduce cs e
    case e' of
        Literal x -> return x
        _         -> throwError (Eval.NotConstant rgn)


-- | Reduce an expression as far as possible by evaluating constant
-- sub-expressions.
reduce :: Constants -> Expr t -> Either Eval.Error (Expr t)
reduce cs e = runReducer (T.transformM toLiteral e) cs


toLiteral :: Expr t -> Reducer (Expr t)
toLiteral e = case e of
    Variable name ty -> do
        value <- view (constants.at name)
        case value of
            Just (Value v ty') -> case typeEq ty ty' of -- if the variable is a constant ...
                Just Refl -> case dictEq ty' of
                    Dict -> return (Literal v) -- ... then replace by constant value
                Nothing -> error "reduce: constant has wrong type"
            Nothing -> return e

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

    EqOp _ eOp (Literal l) (Literal r) ->
        return (Literal (eqOp eOp l r))

    RelOp _ rOp (Literal l) (Literal r) ->
        return (Literal (relOp rOp l r))

    LogicOp lOp (Literal l) (Literal r) ->
        return (Literal (logicOp lOp l r))

    HasType (Literal comp) tyName ->
        return (Literal (view compTypeName comp == tyName))

    BoundTo (Literal role) (Literal player) ->
        return (Literal (view compBoundTo role == Just (view compName player)))

    Element (Literal role) (Literal compartment) ->
        return (Literal
            (view compContainedIn role == Just (view compName compartment)))

    Quantified q mTyName sc -> do
        comps <- components mTyName <$> view constants

        -- instantiate quantified expression and reduce
        es <- traverse (T.transformM toLiteral . instantiate sc) comps

        -- remove all literals that do not influence the truth value
        let es' = filterLiterals q es

        -- connect instantiated expressions
        return (quantifier q es')

    _ -> return e


filterLiterals :: Quantifier -> [Expr Bool] -> [Expr Bool]
filterLiterals q = filter (/= neutralElement q)


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
