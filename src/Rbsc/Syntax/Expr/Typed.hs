{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


-- | Typed expressions.
module Rbsc.Syntax.Expr.Typed
    ( Expr(..)
    , Scope(..)

    , SomeExpr(..)

    , instantiate

    , transform
    , transformM
    , descend
    ) where


import Control.Monad.Identity

import Data.List.NonEmpty (NonEmpty)


import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Operators


-- | Typed abstract syntax of expressions.
data Expr t where
    Literal     :: Show t => t -> Expr t
    Array       :: Show t => NonEmpty (Expr t) -> Expr (Array t)
    LitFunction :: TypedFunction t -> Expr (Fn t)
    Variable    :: Name -> Type t -> Expr t
    Cast        :: Expr Integer -> Expr Double
    Not         :: Expr Bool -> Expr Bool
    Negate      :: Num t => Expr t -> Expr t
    ArithOp     :: Num t => ArithOp -> Expr t -> Expr t -> Expr t
    Divide      :: Region -> Expr Double -> Expr Double -> Expr Double
    EqOp        :: Eq t => EqOp -> Expr t -> Expr t -> Expr Bool
    RelOp       :: Ord t => RelOp -> Expr t -> Expr t -> Expr Bool
    LogicOp     :: LogicOp -> Expr Bool -> Expr Bool -> Expr Bool
    Index       :: Show t => Expr (Array t) -> Loc (Expr Integer) -> Expr t
    Apply       :: Show b => Expr (Fn (a -> b)) -> Expr a -> Expr b
    HasType     :: Expr Component -> TypeName -> Expr Bool
    BoundTo     :: Expr Component -> Expr Component -> Expr Bool
    Element     :: Expr Component -> Expr Component -> Expr Bool
    Bound       :: Int -> Type t -> Expr t
    Lambda      :: Type a -> Scope b -> Expr (Fn (a -> b))
    Quantified  :: Quantifier -> Maybe TypeName -> Scope Bool -> Expr Bool

deriving instance Show (Expr t)


-- | A Scope contains an expression with a bound variable.
--
-- Variables bound by a quantifier are identified by their de Bruijn index.
newtype Scope t = Scope (Expr t) deriving (Show)


-- | An 'Expr' tagged with its 'Type'.
data SomeExpr where
    SomeExpr :: Expr t -> Type t -> SomeExpr

deriving instance Show SomeExpr

-- | Instantiate all variables bound by the outermost binder.
instantiate :: forall t. Scope t -> SomeExpr -> Expr t
instantiate (Scope body) (SomeExpr s ty) = go 0 body
  where
    go :: Int -> Expr a -> Expr a
    go i = \case
        Literal x         -> Literal x
        Array es          -> Array (fmap (go i) es)
        LitFunction f     -> LitFunction f
        Variable name ty' -> Variable name ty'
        Cast e            -> Cast (go i e)
        Not e             -> Not (go i e)
        Negate e          -> Negate (go i e)
        ArithOp aOp l r   -> ArithOp aOp (go i l) (go i r)
        Divide rgn l r    -> Divide rgn (go i l) (go i r)
        EqOp eOp l r      -> EqOp eOp (go i l) (go i r)
        RelOp rOp l r     -> RelOp rOp (go i l) (go i r)
        LogicOp lOp l r   -> LogicOp lOp (go i l) (go i r)
        Index e idx       -> Index (go i e) (fmap (go i) idx)
        Apply f e         -> Apply (go i f) (go i e)
        HasType e tyName  -> HasType (go i e) tyName
        BoundTo l r       -> BoundTo (go i l) (go i r)
        Element l r       -> Element (go i l) (go i r)
        Bound i' ty'
            | i == i' -> case typeEq ty ty' of
                Just Refl -> s
                Nothing   -> error "instantiate: type error"
            | otherwise -> Bound i' ty'
        Lambda ty' (Scope body') -> Lambda ty' (Scope (go (succ i) body'))
        Quantified q mTyName (Scope body') ->
            Quantified q mTyName (Scope (go (succ i) body'))


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
    Literal x        -> pure (Literal x)
    Array es         -> Array <$> traverse f es
    LitFunction g    -> pure (LitFunction g)
    Variable name ty -> pure (Variable name ty)
    Cast e           -> Cast <$> f e
    Not e            -> Not <$> f e
    Negate e         -> Negate <$> f e
    ArithOp aOp l r  -> ArithOp aOp <$> f l <*> f r
    Divide rgn l r   -> Divide rgn <$> f l <*> f r
    EqOp eOp l r     -> EqOp eOp <$> f l <*> f r
    RelOp rOp l r    -> RelOp rOp <$> f l <*> f r
    LogicOp lOp l r  -> LogicOp lOp <$> f l <*> f r
    Index e idx      -> Index <$> f e <*> traverse f idx
    Apply g e        -> Apply <$> f g <*> f e
    HasType e tyName -> HasType <$> f e <*> pure tyName
    BoundTo l r      -> BoundTo <$> f l <*> f r
    Element l r      -> Element <$> f l <*> f r
    Bound i ty       -> pure (Bound i ty)
    Lambda ty (Scope body) -> Lambda ty . Scope <$> f body
    Quantified q mTyName (Scope body) -> Quantified q mTyName . Scope <$> f body
