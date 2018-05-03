{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


-- | Typed expressions.
module Rbsc.Syntax.Expr.Typed
    ( Expr(..)
    , Scoped(..)
    , Quantifier(..)
    , TQuantifiedType

    , SomeExpr(..)

    , Constants

    , instantiate

    , transform
    , transformM
    , descend
    ) where


import Control.Monad.Identity

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict    (Map)
import Data.Set           (Set)


import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Operators
import Rbsc.Syntax.Quantification


-- | Typed abstract syntax of expressions.
data Expr t where
    Literal     :: Show t => t -> Expr t
    LitArray    :: Show t => NonEmpty (Expr t) -> Expr (Array t)
    LitFunction :: TypedFunction t -> Expr (Fn t)
    Self        :: Expr Component
    Identifier  :: Name -> Type t -> Expr t
    Cast        :: Expr Integer -> Expr Double
    Not         :: Expr Bool -> Expr Bool
    Negate      :: Num t => Expr t -> Expr t
    ArithOp     :: Num t => ArithOp -> Expr t -> Expr t -> Expr t
    Divide      :: Region -> Expr Double -> Expr Double -> Expr Double
    EqOp        :: Eq t => EqOp -> Expr t -> Expr t -> Expr Bool
    RelOp       :: Ord t => RelOp -> Expr t -> Expr t -> Expr Bool
    LogicOp     :: LogicOp -> Expr Bool -> Expr Bool -> Expr Bool
    Member      :: Expr Component -> Name -> Type t -> Expr t
    Index       :: Show t => Expr (Array t) -> Loc (Expr Integer) -> Expr t
    Apply       :: Show b => Expr (Fn (a -> b)) -> Expr a -> Expr b
    IfThenElse  :: Expr Bool -> Expr t -> Expr t -> Expr t
    HasType     :: Expr Component -> TypeName -> Expr Bool
    BoundTo     :: Loc (Expr Component) -> Loc (Expr Component) -> Expr Bool
    Element     :: Loc (Expr Component) -> Loc (Expr Component) -> Expr Bool
    Bound       :: Int -> Type t -> Expr t
    Count       :: Set TypeName -> Expr Component -> Expr Integer
    Lambda      :: Type a -> Scoped b -> Expr (Fn (a -> b))
    Quantified  :: Quantifier t -> TQuantifiedType -> Scoped t -> Expr t

deriving instance Show (Expr t)


data Quantifier t where
    Forall  :: Quantifier Bool
    Exists  :: Quantifier Bool
    Sum     :: Quantifier Integer
    Product :: Quantifier Integer

deriving instance Show (Quantifier t)


type TQuantifiedType = QuantifiedType (Set TypeName) (Expr Integer)


-- | A scoped expression contains a bound variable.
--
-- Variables bound by a quantifier are identified by their de Bruijn index.
newtype Scoped t = Scoped (Expr t) deriving (Show)


-- | An 'Expr' tagged with its 'Type'.
data SomeExpr where
    SomeExpr :: Expr t -> Type t -> SomeExpr

deriving instance Show SomeExpr


-- | The table of constants.
type Constants = Map Name SomeExpr


-- | Instantiate all variables bound by the outermost binder.
instantiate :: forall t. Scoped t -> SomeExpr -> Expr t
instantiate (Scoped body) (SomeExpr s ty) = go 0 body
  where
    go :: Int -> Expr a -> Expr a
    go i = \case
        Literal x           -> Literal x
        LitArray es         -> LitArray (fmap (go i) es)
        LitFunction f       -> LitFunction f
        Self                -> Self
        Identifier name ty' -> Identifier name ty'
        Cast e              -> Cast (go i e)
        Not e               -> Not (go i e)
        Negate e            -> Negate (go i e)
        ArithOp aOp l r     -> ArithOp aOp (go i l) (go i r)
        Divide rgn l r      -> Divide rgn (go i l) (go i r)
        EqOp eOp l r        -> EqOp eOp (go i l) (go i r)
        RelOp rOp l r       -> RelOp rOp (go i l) (go i r)
        LogicOp lOp l r     -> LogicOp lOp (go i l) (go i r)
        Member e name ty'   -> Member (go i e) name ty'
        Index e idx         -> Index (go i e) (fmap (go i) idx)
        Apply f e           -> Apply (go i f) (go i e)
        IfThenElse c t e    -> IfThenElse (go i c) (go i t) (go i e)
        HasType e tyName    -> HasType (go i e) tyName
        BoundTo l r         -> BoundTo (fmap (go i) l) (fmap (go i) r)
        Element l r         -> Element (fmap (go i) l) (fmap (go i) r)
        Bound i' ty'
            | i == i' -> case typeEq ty ty' of
                Just Refl -> s
                Nothing   -> error "instantiate: type error"
            | otherwise -> Bound i' ty'
        Count tySet e -> Count tySet (go i e)
        Lambda ty' (Scoped body') -> Lambda ty' (Scoped (go (succ i) body'))
        Quantified q qdTy (Scoped body') ->
            Quantified q (goQdType i qdTy) (Scoped (go (succ i) body'))

    goQdType i = \case
        QdTypeComponent tySet    -> QdTypeComponent tySet
        QdTypeInt (lower, upper) -> QdTypeInt (go i lower, go i upper)


-- | Transform every element in an expression tree, in a bottom-up manner.
transform :: (forall a. Expr a -> Expr a) -> Expr t -> Expr t
transform f = runIdentity . transformM (Identity . f)


-- | Transform every element in an expression tree, in a bottom-up manner
-- and monadically.
transformM ::
       forall m.
       forall t. Monad m =>
                     (forall a. Expr a -> m (Expr a)) -> Expr t -> m (Expr t)
transformM f = go
  where
    go :: Expr t -> m (Expr t)
    go e = descend go e >>= f


-- | Traverse the children of an 'Expr'.
descend ::
       Applicative m => (forall a. Expr a -> m (Expr a)) -> Expr t -> m (Expr t)
descend f = \case
    Literal x          -> pure (Literal x)
    LitArray es        -> LitArray <$> traverse f es
    LitFunction g      -> pure (LitFunction g)
    Self               -> pure Self
    Identifier name ty -> pure (Identifier name ty)
    Cast e             -> Cast <$> f e
    Not e              -> Not <$> f e
    Negate e           -> Negate <$> f e
    ArithOp aOp l r    -> ArithOp aOp <$> f l <*> f r
    Divide rgn l r     -> Divide rgn <$> f l <*> f r
    EqOp eOp l r       -> EqOp eOp <$> f l <*> f r
    RelOp rOp l r      -> RelOp rOp <$> f l <*> f r
    LogicOp lOp l r    -> LogicOp lOp <$> f l <*> f r
    Member e name ty   -> Member <$> f e <*> pure name <*> pure ty
    Index e idx        -> Index <$> f e <*> traverse f idx
    Apply g e          -> Apply <$> f g <*> f e
    IfThenElse c t e   -> IfThenElse <$> f c <*> f t <*> f e
    HasType e tyName   -> HasType <$> f e <*> pure tyName
    BoundTo l r        -> BoundTo <$> traverse f l <*> traverse f r
    Element l r        -> Element <$> traverse f l <*> traverse f r
    Bound i ty         -> pure (Bound i ty)
    Count tySet e      -> Count tySet <$> f e
    Lambda ty (Scoped body) -> Lambda ty . Scoped <$> f body
    Quantified q qdTy@(QdTypeComponent _) (Scoped body) ->
        Quantified q qdTy . Scoped <$> f body
    Quantified q (QdTypeInt (lower, upper)) (Scoped body) ->
        Quantified q
            <$> (QdTypeInt <$> ((,) <$> f lower <*> f upper))
            <*> (Scoped <$> f body)
