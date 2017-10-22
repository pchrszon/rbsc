{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}


-- | Evaluation of typed expressions.
module Rbsc.Eval
    ( eval
    ) where


import Control.Lens
import Control.Monad.Reader

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Rbsc.Syntax.Expr.Typed
import Rbsc.Syntax.Operators
import Rbsc.Type
import Rbsc.Value


-- | Evaluates an expression under a given set of 'Constants'.
eval :: MonadReader Constants m => Expr t -> m t
eval = \case
    Literal x -> return x
    Variable name ty -> do
        consts <- ask
        case Map.lookup name consts of
            Just (Value ty' v) ->
                case typeEq ty ty' of
                    Just Refl -> return v
                    Nothing -> error "type mismatch"
            Nothing -> error "constant not found"
    Not c -> not <$> eval c
    BoolBinOp binOp l r -> boolBinOp binOp <$> eval l <*> eval r
    HasType c tyName -> do
        comp <- eval c
        return (view compTypeName comp == tyName)
    BoundTo l r -> do
        role <- eval l
        player <- eval r
        return (view compBoundTo role == Just (view compName player))
    Element l r -> do
        role <- eval l
        compartment <- eval r
        return
            (view compContainedIn role == Just (view compName compartment))
    Quantified q mTyName sc -> do
        comps <- components mTyName <$> ask
        bs <- traverse (eval . instantiate sc) comps
        return (quantifier q bs)
    Bound _ -> error "unbound variable"


-- | Get the list of all constants of type 'Component'. If a 'TypeName' is
-- given, only the 'Component's of this type are returned.
components :: Maybe TypeName -> Constants -> [Component]
components mTyName = mapMaybe getComponent . Map.elems
  where
    getComponent :: Value -> Maybe Component
    getComponent = \case
        Value (TyComponent tyName _) comp
            | matchType tyName -> Just comp
        _ -> Nothing

    matchType tyName =
        case mTyName of
            Just tyName' -> tyName == Just tyName'
            Nothing      -> True
