{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


-- | Typed expressions.
module Rbsc.Syntax.Expr.Typed
    ( Expr(..)
    , Scope(..)

    , instantiate
    , transform
    , transformM
    ) where


import Control.Monad.Identity

import Data.List.NonEmpty (NonEmpty)


import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Operators


-- | Typed abstract syntax of expressions.
data Expr t where
    Literal    :: (Eq t, Show t) => t -> Expr t
    Array      :: (Eq t, Show t) => NonEmpty (Expr t) -> Expr (Array t)
    Variable   :: Name -> Type t -> Expr t
    Cast       :: Expr Integer -> Expr Double
    Not        :: Expr Bool -> Expr Bool
    Negate     :: Num t => Expr t -> Expr t
    ArithOp    :: Num t => ArithOp -> Expr t -> Expr t -> Expr t
    Divide     :: Region -> Expr Double -> Expr Double -> Expr Double
    EqOp       :: Eq t => Type t -> EqOp -> Expr t -> Expr t -> Expr Bool
    RelOp      :: Ord t => Type t -> RelOp -> Expr t -> Expr t -> Expr Bool
    LogicOp    :: LogicOp -> Expr Bool -> Expr Bool -> Expr Bool
    Index      :: (Eq t, Show t) => Expr (Array t) -> Loc (Expr Integer) -> Expr t
    HasType    :: Expr Component -> TypeName -> Expr Bool
    BoundTo    :: Expr Component -> Expr Component -> Expr Bool
    Element    :: Expr Component -> Expr Component -> Expr Bool
    Bound      :: Int -> Expr Component
    Quantified :: Quantifier -> Maybe TypeName -> Scope Bool -> Expr Bool

deriving instance Show (Expr t)

instance Eq (Expr t) where
    Literal x == Literal x' =
        x == x'

    Array es == Array es' =
        es == es'

    Variable name ty == Variable name' ty' =
        name == name' && ty == ty'

    Cast e == Cast e' =
        e == e'

    Not e == Not e' =
        e == e'

    Negate e == Negate e' =
        e == e'

    ArithOp aOp l r == ArithOp aOp' l' r' =
        aOp == aOp' && l == l' && r == r'

    Divide _ l r == Divide _ l' r' =
        l == l' && r == r'

    EqOp ty eOp l r == EqOp ty' eOp' l' r' = case typeEq ty ty' of
        Just Refl -> eOp == eOp' && l == l' && r == r'
        Nothing   -> False

    RelOp ty rOp l r == RelOp ty' rOp' l' r' = case typeEq ty ty' of
        Just Refl -> rOp == rOp' && l == l' && r == r'
        Nothing   -> False

    LogicOp lOp l r == LogicOp lOp' l' r' =
        lOp == lOp' && l == l' && r == r'

    Index e (Loc idx _) == Index e' (Loc idx' _) =
        e == e' && idx == idx'

    HasType e tyName == HasType e' tyName' =
        e == e' && tyName == tyName'

    BoundTo l r == BoundTo l' r' =
        l == l' && r == r'

    Element l r == Element l' r' =
        l == l' && r == r'

    Bound i == Bound i' =
        i == i'

    Quantified q mTyName body == Quantified q' mTyName' body' =
        q == q' && mTyName == mTyName' && body == body'

    _ == _ = False


-- | A Scope contains an expression with a bound variable.
--
-- Variables bound by a quantifier are identified by their de Bruijn index.
newtype Scope t = Scope (Expr t) deriving (Eq, Show)


-- | Instantiate all variables bound by the outermost quantifier.
instantiate :: forall t. Scope t -> Component -> Expr t
instantiate (Scope body) comp = go 0 body
  where
    go :: Int -> Expr a -> Expr a
    go i = \case
        Literal x        -> Literal x
        Array es         -> Array (fmap (go i) es)
        Variable name ty -> Variable name ty
        Cast e           -> Cast (go i e)
        Not e            -> Not (go i e)
        Negate e         -> Negate (go i e)
        ArithOp aOp l r  -> ArithOp aOp (go i l) (go i r)
        Divide rgn l r   -> Divide rgn (go i l) (go i r)
        EqOp ty eOp l r  -> EqOp ty eOp (go i l) (go i r)
        RelOp ty rOp l r -> RelOp ty rOp (go i l) (go i r)
        LogicOp lOp l r  -> LogicOp lOp (go i l) (go i r)
        Index e idx      -> Index (go i e) (fmap (go i) idx)
        HasType e tyName -> HasType (go i e) tyName
        BoundTo l r      -> BoundTo (go i l) (go i r)
        Element l r      -> Element (go i l) (go i r)
        Bound i'
            | i == i' -> Literal comp
            | otherwise -> Bound i'
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


descend ::
       Applicative m => (forall a. Expr a -> m (Expr a)) -> Expr t -> m (Expr t)
descend f = \case
    Literal x        -> pure (Literal x)
    Array es         -> Array <$> traverse f es
    Variable name ty -> pure (Variable name ty)
    Cast e           -> Cast <$> f e
    Not e            -> Not <$> f e
    Negate e         -> Negate <$> f e
    ArithOp aOp l r  -> ArithOp aOp <$> f l <*> f r
    Divide rgn l r   -> Divide rgn <$> f l <*> f r
    EqOp ty eOp l r  -> EqOp ty eOp <$> f l <*> f r
    RelOp ty rOp l r -> RelOp ty rOp <$> f l <*> f r
    LogicOp lOp l r  -> LogicOp lOp <$> f l <*> f r
    Index e idx      -> Index <$> f e <*> traverse f idx
    HasType e tyName -> HasType <$> f e <*> pure tyName
    BoundTo l r      -> BoundTo <$> f l <*> f r
    Element l r      -> Element <$> f l <*> f r
    Bound i          -> pure (Bound i)
    Quantified q mTyName (Scope body) -> Quantified q mTyName . Scope <$> f body
