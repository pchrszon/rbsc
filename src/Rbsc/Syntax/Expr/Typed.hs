{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Typed expressions.
module Rbsc.Syntax.Expr.Typed
    ( Expr(..)
    , Scope(..)
    , instantiate
    ) where


import Rbsc.Component
import Rbsc.Name
import Rbsc.Syntax.Operators
import Rbsc.Type


-- | Typed abstract syntax of expressions.
data Expr t where
    Literal    :: t -> Expr t
    Variable   :: Name -> Type t -> Expr t
    Not        :: Expr Bool -> Expr Bool
    BoolBinOp  :: BoolBinOp -> Expr Bool -> Expr Bool -> Expr Bool
    HasType    :: Expr Component -> TypeName -> Expr Bool
    BoundTo    :: Expr Component -> Expr Component -> Expr Bool
    Element    :: Expr Component -> Expr Component -> Expr Bool
    Quantified :: Quantifier -> Maybe TypeName -> Scope Bool -> Expr Bool
    Bound      :: Int -> Expr Component

deriving instance Show t => Show (Expr t)
deriving instance Eq t => Eq (Expr t)


-- | A Scope contains an expression with a bound variable.
--
-- Variables bound by a quantifier are identified by their de Bruijn index.
newtype Scope t = Scope (Expr t) deriving (Eq, Show)


-- | Instantiate all variables bound by the outermost quantifier.
instantiate :: Scope t -> Component -> Expr t
instantiate (Scope body) comp = go 0 body
  where
    go :: Int -> Expr t -> Expr t
    go i = \case
        Literal x -> Literal x
        Variable name ty -> Variable name ty
        Not e -> Not (go i e)
        BoolBinOp binOp l r -> BoolBinOp binOp (go i l) (go i r)
        HasType e tyName -> HasType (go i e) tyName
        BoundTo l r -> BoundTo (go i l) (go i r)
        Element l r -> Element (go i l) (go i r)
        Quantified q mTyName (Scope body') ->
            Quantified q mTyName (Scope (go (succ i) body'))
        Bound i'
            | i == i' -> Literal comp
            | otherwise -> Bound i'
