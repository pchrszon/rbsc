{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


-- | Typed expressions.
module Rbsc.Syntax.Typed.Expr
    ( Expr(..)
    , Scoped(..)
    , Quantifier(..)
    , TQuantifiedType

    , SomeExpr(..)

    , Constants
    , constants
    , isConstant
    , prettyConstants

    , Methods
    , methods

    , HasExprs(..)

    , instantiate
    , instantiateUnder

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

import           Data.Foldable
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Set                  (Set)
import           Data.Text.Prettyprint.Doc


import Rbsc.Data.Action
import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.Field
import Rbsc.Data.Function
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Operators
import Rbsc.Syntax.Quantification


-- | Typed abstract syntax of expressions.
data Expr t where
    Literal        :: Show t => t -> Type t -> Expr t
    LitFunction    :: TypedFunction t -> Expr (Fn t)
    LitArray       :: Show t => NonEmpty (Expr t) -> Expr (Array t)
    GenArray       :: Show t => Expr t -> Int -> Int -> Expr (Array t)
    Self           :: Expr Component
    Identifier     :: Name -> Type t -> Expr t
    Cast           :: Expr Int -> Expr Double
    ActionArray    :: Expr Action -> Expr (Array Action)
    IsPlayed       :: Loc (Expr Component) -> Expr Bool
    Not            :: Expr Bool -> Expr Bool
    Negate         :: Num t => Expr t -> Expr t
    ArithOp        :: (Eq t, Num t) => ArithOp -> Expr t -> Expr t -> Expr t
    Divide         :: Region -> Expr Double -> Expr Double -> Expr Double
    EqOp           :: Eq t => EqOp -> Type t -> Expr t -> Expr t -> Expr Bool
    RelOp          :: Ord t => RelOp -> Expr t -> Expr t -> Expr Bool
    LogicOp        :: LogicOp -> Expr Bool -> Expr Bool -> Expr Bool
    Member         :: Expr Component -> Name -> Type t -> Expr t
    Index          :: Show t => Expr (Array t) -> Maybe Int -> Loc (Expr Int) -> Expr t
    Apply          :: Show b => Expr (Fn (a -> b)) -> Expr a -> Expr b
    IfThenElse     :: Expr Bool -> Expr t -> Expr t -> Expr t
    HasType        :: Expr Component -> Set TypeName -> Expr Bool
    BoundTo        :: Loc (Expr Component) -> Loc (Expr Component) -> Expr Bool
    Element        :: Loc (Expr Component) -> Loc (Expr Component) -> Expr Bool
    Bound          :: Int -> Type t -> Expr t
    Count          :: Set TypeName -> Expr Component -> Expr Int
    Player         :: Loc (Expr Component) -> Expr Component
    Playable       :: Loc (Expr Component) -> Maybe (Loc (Expr Action)) -> Expr Bool
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


-- | A 'Lens' for accessing the 'Constants'.
constants :: Has Constants r => Lens' r Constants
constants = field


-- | Returns @True@ if there is a constant with the given name.
isConstant :: Constants -> Name -> Bool
isConstant consts name = Map.member name consts


-- | Pretty-print the table of 'Constants'.
prettyConstants :: Constants -> [Doc ann]
prettyConstants = fmap prettyConstant . filter (isValue . snd) . Map.assocs
  where
    isValue (SomeExpr _ ty) = case ty of
        TyComponent _             -> False
        TyArray _ (TyComponent _) -> False
        TyFunc _ _                -> False
        _                         -> True

    prettyConstant (name, SomeExpr e ty) =
        pretty name <+> colon <+> pretty ty <+> equals <+>
        nest 4 (prettyLiteral e)

    prettyLiteral = \case
        Literal x ty' -> prettyValue x ty'
        Lambda _ _    -> "<function>"
        _             -> "<non-printable value>"

    prettyValue :: Show t => t -> Type t -> Doc ann
    prettyValue b TyBool
        | b         = "true"
        | otherwise = "false"
    prettyValue arr  (TyArray _ ty') = case dictShow ty' of
        Dict ->
            list (fmap (uncurry prettyValue) (zip (toList arr) (repeat ty')))
    prettyValue act TyAction   = pretty act
    prettyValue x _            = viaShow x


-- | The table of methods.
type Methods = Map ScopedName SomeExpr


-- | A 'Lens' for accessing the 'Methods'.
methods :: Has Methods r => Lens' r Methods
methods = field


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


-- | @instantiate body e@ substitutes all variables in @body@ bound by the
-- outermost binder with @e@.
instantiate :: Scoped t -> SomeExpr -> Expr t
instantiate = instantiateUnder 0


-- | @instantiateUnder i body e@ substitutes all variables in @body@ bound
-- by the outermost binder with @e@, where @i@ is the number of binders
-- above.
instantiateUnder :: Int -> Scoped t -> SomeExpr -> Expr t
instantiateUnder o (Scoped body) (SomeExpr s ty) = runReader (go body) o
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
        GenArray gen l u ->
            GenArray <$> local succ (go gen) <*> pure l <*> pure u
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
    LitFunction g      -> pure (LitFunction g)
    LitArray es        -> LitArray <$> traverse f es
    GenArray e l u     -> GenArray <$> f e <*> pure l <*> pure u
    Self               -> pure Self
    Identifier name ty -> pure (Identifier name ty)
    Cast e             -> Cast <$> f e
    ActionArray e      -> ActionArray <$> f e
    IsPlayed e         -> IsPlayed <$> traverse f e
    Not e              -> Not <$> f e
    Negate e           -> Negate <$> f e
    ArithOp aOp l r    -> ArithOp aOp <$> f l <*> f r
    Divide rgn l r     -> Divide rgn <$> f l <*> f r
    EqOp eOp ty l r    -> EqOp eOp ty <$> f l <*> f r
    RelOp rOp l r      -> RelOp rOp <$> f l <*> f r
    LogicOp lOp l r    -> LogicOp lOp <$> f l <*> f r
    Member e name ty   -> Member <$> f e <*> pure name <*> pure ty
    Index e mSize idx  -> Index <$> f e <*> pure mSize <*> traverse f idx
    Apply g e          -> Apply <$> f g <*> f e
    IfThenElse c t e   -> IfThenElse <$> f c <*> f t <*> f e
    HasType e tySet    -> HasType <$> f e <*> pure tySet
    BoundTo l r        -> BoundTo <$> traverse f l <*> traverse f r
    Element l r        -> Element <$> traverse f l <*> traverse f r
    Bound i ty         -> pure (Bound i ty)
    Count tySet e      -> Count tySet <$> f e
    Player e           -> Player <$> traverse f e
    Playable e mAct    -> Playable
        <$> traverse f e
        <*> traverse (traverse f) mAct
    ComponentIndex e -> ComponentIndex <$> traverse f e
    Lambda ty (Scoped body) -> Lambda ty . Scoped <$> f body
    Quantified q qdTy@(QdTypeComponent _) (Scoped body) ->
        Quantified q qdTy . Scoped <$> f body
    Quantified q (QdTypeInt (lower, upper)) (Scoped body) ->
        Quantified q
            <$> (QdTypeInt <$> ((,) <$> traverse f lower <*> traverse f upper))
            <*> (Scoped <$> f body)
