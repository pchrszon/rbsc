{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


-- | Typed expressions.
module Rbsc.Syntax.Typed.Expr
    ( HasExprs(..)

    , Expr(..)
    , Scoped(..)
    , Quantifier(..)
    , TQuantifiedType

    , SomeExpr(..)

    , Constants
    , HasConstants(..)
    , isConstant

    , instantiate
    , instantiateExprs

    , transformExpr
    , transformExprM
    , transformExprs
    , transformExprsM

    , universeExpr
    , universeExprs

    , plateExpr
    ) where


import Control.Applicative
import Control.Lens
import Control.Monad.Reader

import           Data.List.NonEmpty (NonEmpty)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Monoid
import           Data.Set           (Set)


import Rbsc.Data.Action
import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Operators
import Rbsc.Syntax.Quantification


-- | Any syntax tree node @n@ that 'HasExprs' provides a traversal of all
-- 'Expr's contained in that node and its subtree.
class HasExprs a where
    exprs ::
           Applicative f
        => (forall t. Loc (Expr t) -> f (Loc (Expr t)))
        -> a
        -> f a


instance HasExprs (Loc SomeExpr) where
    exprs f (Loc (SomeExpr e ty) rgn) = fromLocExpr <$> f (Loc e rgn)
      where
        fromLocExpr (Loc e' rgn') = Loc (SomeExpr e' ty) rgn'


-- | Typed abstract syntax of expressions.
data Expr t where
    Literal        :: Show t => t -> Type t -> Expr t
    LitArray       :: Show t => NonEmpty (Expr t) -> Expr (Array t)
    LitFunction    :: TypedFunction t -> Expr (Fn t)
    Self           :: Expr Component
    Identifier     :: Name -> Type t -> Expr t
    Cast           :: Expr Int -> Expr Double
    ActionArray    :: Expr Action -> Expr (Array Action)
    IsPlayed       :: Expr Component -> Expr Bool
    Not            :: Expr Bool -> Expr Bool
    Negate         :: Num t => Expr t -> Expr t
    ArithOp        :: Num t => ArithOp -> Expr t -> Expr t -> Expr t
    Divide         :: Region -> Expr Double -> Expr Double -> Expr Double
    EqOp           :: Eq t => EqOp -> Type t -> Expr t -> Expr t -> Expr Bool
    RelOp          :: Ord t => RelOp -> Expr t -> Expr t -> Expr Bool
    LogicOp        :: LogicOp -> Expr Bool -> Expr Bool -> Expr Bool
    Member         :: Expr Component -> Name -> Type t -> Expr t
    Index          :: Show t => Expr (Array t) -> Loc (Expr Int) -> Expr t
    Apply          :: Show b => Expr (Fn (a -> b)) -> Expr a -> Expr b
    IfThenElse     :: Expr Bool -> Expr t -> Expr t -> Expr t
    HasType        :: Expr Component -> TypeName -> Expr Bool
    BoundTo        :: Loc (Expr Component) -> Loc (Expr Component) -> Expr Bool
    Element        :: Loc (Expr Component) -> Loc (Expr Component) -> Expr Bool
    Bound          :: Int -> Type t -> Expr t
    Count          :: Set TypeName -> Expr Component -> Expr Int
    HasPlayer      :: Expr Component -> Expr Bool
    Player         :: Loc (Expr Component) -> Expr Component
    ComponentIndex :: Loc (Expr Component) -> Expr Int
    Lambda         :: Type a -> Scoped b -> Expr (Fn (a -> b))
    Quantified     :: Quantifier t -> TQuantifiedType -> Scoped t -> Expr t

deriving instance Show (Expr t)


data Quantifier t where
    Forall  :: Quantifier Bool
    Exists  :: Quantifier Bool
    Sum     :: Quantifier Int
    Product :: Quantifier Int

deriving instance Show (Quantifier t)


type TQuantifiedType = QuantifiedType (Set TypeName) (Loc (Expr Int))

instance HasExprs (QuantifiedType ty (Loc (Expr Int))) where
    exprs f = \case
        QdTypeComponent c -> pure (QdTypeComponent c)
        QdTypeInt (lower, upper) ->
            QdTypeInt <$> ((,) <$> f lower <*> f upper)


-- | A scoped expression contains a bound variable.
--
-- Variables bound by a quantifier are identified by their de Bruijn index.
newtype Scoped t = Scoped (Expr t) deriving (Show)


-- | An 'Expr' tagged with its 'Type'.
data SomeExpr where
    SomeExpr :: Expr t -> Type t -> SomeExpr

deriving instance Show SomeExpr

deriving instance Show (Some Expr)


-- | The table of constants.
type Constants = Map Name SomeExpr

class HasConstants a where
    constants :: Lens' a Constants


-- | Returns @True@ if there is a constant with the given name.
isConstant :: Constants -> Name -> Bool
isConstant consts name = Map.member name consts


-- | Instantiate all variables bound by the outermost binder.
instantiate :: Scoped t -> SomeExpr -> Expr t
instantiate (Scoped body) (SomeExpr s ty) = runReader (go body) 0
  where
    go :: Expr a -> Reader Int (Expr a)
    go = \case
        Bound i' ty' -> do
            i <- ask
            if i == i'
                then case typeEq ty ty' of
                    Just Refl -> return s
                    Nothing   -> error "instantiate: type error"
                else
                    return (Bound i' ty')
        Lambda ty' (Scoped body') ->
            Lambda ty' . Scoped <$> local succ (go body')
        Quantified q qdTy (Scoped body') -> Quantified q
            <$> goQdType qdTy
            <*> (Scoped <$> local succ (go body'))
        e -> plateExpr go e

    goQdType = \case
        QdTypeComponent tySet -> return (QdTypeComponent tySet)
        QdTypeInt (lower, upper) ->
            QdTypeInt <$> ((,) <$> traverse go lower <*> traverse go upper)


-- | Instantiate the outermost binder in all expressions in a syntax tree.
instantiateExprs :: HasExprs a => SomeExpr -> a -> a
instantiateExprs s = transformExprs (\e -> instantiate (Scoped e) s)


-- | Transform every element in an expression tree, in a bottom-up manner.
transformExpr :: (forall a. Expr a -> Expr a) -> Expr t -> Expr t
transformExpr f = runIdentity . transformExprM (Identity . f)


-- | For each expression, transform every element in the expression tree,
-- in a bottom-up manner and monadically.
transformExprsM
    :: forall m
     . forall a
     . (HasExprs a, Monad m)
    => (forall t. Expr t -> m (Expr t))
    -> a
    -> m a
transformExprsM f = exprs trans
  where
    trans :: Loc (Expr t) -> m (Loc (Expr t))
    trans (Loc e rgn) = do
        e' <- transformExprM f e
        return (Loc e' rgn)


-- | Transform every element in an expression tree, in a bottom-up manner
-- and monadically.
transformExprM
    :: forall m
     . forall t
     . Monad m
    => (forall a. Expr a -> m (Expr a))
    -> Expr t
    -> m (Expr t)
transformExprM f = go
  where
    go :: Expr t -> m (Expr t)
    go e = plateExpr go e >>= f


-- | Transform every expression in a syntax tree node.
transformExprs ::
       HasExprs a => (forall t. Expr t -> Expr t) -> a -> a
transformExprs f = runIdentity . exprs (Identity . fmap (transformExpr f))


-- | Retrieve all of the transitive descendants of an 'Expr', including
-- itself.
universeExpr :: Expr t -> [Some Expr]
universeExpr e = Some e : getConst (plateExpr (Const . universeExpr) e)


-- | Retrieve all 'Expr's and their transitive descendants.
universeExprs :: HasExprs a => a -> [Some Expr]
universeExprs = concat . flip appEndo [] .
    getConst . exprs (Const . Endo . (:) . universeExpr . unLoc)


-- | Traverse the immediate children of an 'Expr'.
plateExpr ::
       Applicative m => (forall a. Expr a -> m (Expr a)) -> Expr t -> m (Expr t)
plateExpr f = \case
    Literal x ty       -> pure (Literal x ty)
    LitArray es        -> LitArray <$> traverse f es
    LitFunction g      -> pure (LitFunction g)
    Self               -> pure Self
    Identifier name ty -> pure (Identifier name ty)
    Cast e             -> Cast <$> f e
    ActionArray e      -> ActionArray <$> f e
    IsPlayed e         -> IsPlayed <$> f e
    Not e              -> Not <$> f e
    Negate e           -> Negate <$> f e
    ArithOp aOp l r    -> ArithOp aOp <$> f l <*> f r
    Divide rgn l r     -> Divide rgn <$> f l <*> f r
    EqOp eOp ty l r    -> EqOp eOp ty <$> f l <*> f r
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
    HasPlayer e        -> HasPlayer <$> f e
    Player e           -> Player <$> traverse f e
    ComponentIndex e   -> ComponentIndex <$> traverse f e
    Lambda ty (Scoped body) -> Lambda ty . Scoped <$> f body
    Quantified q qdTy@(QdTypeComponent _) (Scoped body) ->
        Quantified q qdTy . Scoped <$> f body
    Quantified q (QdTypeInt (lower, upper)) (Scoped body) ->
        Quantified q
            <$> (QdTypeInt <$> ((,) <$> traverse f lower <*> traverse f upper))
            <*> (Scoped <$> f body)
