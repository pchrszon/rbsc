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
    Literal    :: (Eq t, Show t) => t -> Expr t
    Variable   :: Name -> Type t -> Expr t
    Cast       :: Expr Integer -> Expr Double
    Not        :: Expr Bool -> Expr Bool
    Negate     :: Num t => Expr t -> Expr t
    ArithOp    :: Num t => ArithOp -> Expr t -> Expr t -> Expr t
    DivInt     :: Expr Integer -> Expr Integer -> Expr Integer
    DivDouble  :: Expr Double -> Expr Double -> Expr Double
    EqOp       :: Eq t => Type t -> EqOp -> Expr t -> Expr t -> Expr Bool
    RelOp      :: Ord t => Type t -> RelOp -> Expr t -> Expr t -> Expr Bool
    LogicOp    :: LogicOp -> Expr Bool -> Expr Bool -> Expr Bool
    HasType    :: Expr Component -> TypeName -> Expr Bool
    BoundTo    :: Expr Component -> Expr Component -> Expr Bool
    Element    :: Expr Component -> Expr Component -> Expr Bool
    Quantified :: Quantifier -> Maybe TypeName -> Scope Bool -> Expr Bool
    Bound      :: Int -> Expr Component

deriving instance Show (Expr t)

instance Eq (Expr t) where
    Literal x == Literal x' =
        x == x'

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

    DivInt l r == DivInt l' r' =
        l == l' && r == r'

    DivDouble l r == DivDouble l' r' =
        l == l' && r == r'

    EqOp ty eOp l r == EqOp ty' eOp' l' r' = case typeEq ty ty' of
        Just Refl -> eOp == eOp' && l == l' && r == r'
        Nothing   -> False

    RelOp ty rOp l r == RelOp ty' rOp' l' r' = case typeEq ty ty' of
        Just Refl -> rOp == rOp' && l == l' && r == r'
        Nothing   -> False

    LogicOp lOp l r == LogicOp lOp' l' r' =
        lOp == lOp' && l == l' && r == r'

    HasType e tyName == HasType e' tyName' =
        e == e' && tyName == tyName'

    BoundTo l r == BoundTo l' r' =
        l == l' && r == r'

    Element l r == Element l' r' =
        l == l' && r == r'

    Quantified q mTyName body == Quantified q' mTyName' body' =
        q == q' && mTyName == mTyName' && body == body'

    Bound i == Bound i' =
        i == i'

    _ == _ = False


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
        Literal x         -> Literal x
        Variable name ty  -> Variable name ty
        Cast e            -> Cast (go i e)
        Not e             -> Not (go i e)
        Negate e          -> Negate (go i e)
        ArithOp aOp l r   -> ArithOp aOp (go i l) (go i r)
        DivInt l r        -> DivInt (go i l) (go i r)
        DivDouble l r     -> DivDouble (go i l) (go i r)
        EqOp ty eOp l r   -> EqOp ty eOp (go i l) (go i r)
        RelOp ty rOp l r  -> RelOp ty rOp (go i l) (go i r)
        LogicOp binOp l r -> LogicOp binOp (go i l) (go i r)
        HasType e tyName  -> HasType (go i e) tyName
        BoundTo l r       -> BoundTo (go i l) (go i r)
        Element l r       -> Element (go i l) (go i r)
        Quantified q mTyName (Scope body') ->
            Quantified q mTyName (Scope (go (succ i) body'))
        Bound i'
            | i == i' -> Literal comp
            | otherwise -> Bound i'
