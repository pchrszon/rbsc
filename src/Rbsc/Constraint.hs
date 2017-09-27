{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}


-- | Syntax and semantics of constraints.
module Rbsc.Constraint
    (
    -- * Values and constants
      Value(..)
    , Constants

    -- * Operators and quantifiers
    , BoolBinOp(..)
    , boolBinOp
    , Quantifier(..)
    , quantifier

    -- * Constraints
    , Constraint(..)
    , Scope(..)

    -- * Evaluation
    , eval
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

import Rbsc.Types


-- | A value tagged with its 'Type'.
data Value where
    Value :: Show t => Type t -> t -> Value

deriving instance Show Value


-- | A constant has a 'Name' and a 'Value'.
type Constants = Map Name Value


-- | Boolean binary operators.
data BoolBinOp
    = And
    | Or
    deriving (Show)


-- | Semantics of a 'BoolBinOp'.
boolBinOp :: BoolBinOp -> Bool -> Bool -> Bool
boolBinOp = \case
    And -> (&&)
    Or  -> (||)


-- | A quantifier.
data Quantifier
    = Forall
    | Exists
    deriving (Show)


-- | Semantics of a 'Quantifier'.
quantifier :: Foldable t => Quantifier -> t Bool -> Bool
quantifier = \case
    Forall -> and
    Exists -> or


-- | Typed abstract syntax of constraints.
data Constraint t where
    Literal    :: t -> Constraint t
    Variable   :: Name -> Type t -> Constraint t
    Not        :: Constraint Bool -> Constraint Bool
    BoolBinOp  :: BoolBinOp -> Constraint Bool -> Constraint Bool -> Constraint Bool
    HasType    :: Constraint Component -> TypeName -> Constraint Bool
    BoundTo    :: Constraint Component -> Constraint Component -> Constraint Bool
    Element    :: Constraint Component -> Constraint Component -> Constraint Bool
    Quantifier :: Quantifier -> Maybe TypeName -> Scope Bool -> Constraint Bool
    Bound      :: Int -> Constraint Component

deriving instance Show t => Show (Constraint t)


-- | A Scope contains contains a constraint with a bound variable.
--
-- Variables bound by a quantifier are identified by their de Bruijn index.
newtype Scope t = Scope (Constraint t) deriving (Show)


-- | Evaluates a constraint under a given set of 'Constants'.
eval :: MonadReader Constants m => Constraint t -> m t
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
    Quantifier q mTyName sc -> do
        comps <- components mTyName <$> ask
        bs <- traverse (eval . instantiate sc) comps
        return (quantifier q bs)
    Bound _ -> error "unbound variable"


-- | Instantiate all variables bound by the outermost quantifier.
instantiate :: Scope t -> Component -> Constraint t
instantiate (Scope body) comp = go 0 body
  where
    go :: Int -> Constraint t -> Constraint t
    go i = \case
        Literal x -> Literal x
        Variable name ty -> Variable name ty
        Not c -> Not (go i c)
        BoolBinOp binOp l r -> BoolBinOp binOp (go i l) (go i r)
        HasType c tyName -> HasType (go i c) tyName
        BoundTo l r -> BoundTo (go i l) (go i r)
        Element l r -> Element (go i l) (go i r)
        Quantifier q mTyName (Scope body') ->
            Quantifier q mTyName (Scope (go (succ i) body'))
        Bound i'
            | i == i' -> Literal comp
            | otherwise -> Bound i'


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
            Just tyName' -> tyName == tyName'
            Nothing      -> True
