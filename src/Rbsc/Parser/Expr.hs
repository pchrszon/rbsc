{-# LANGUAGE RankNTypes #-}


-- | Parser for expressions.
module Rbsc.Parser.Expr
    ( expr
    ) where


import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Data.Text          (Text)

import Text.Megaparsec
import Text.Megaparsec.Expr


import           Rbsc.Parser.Lexer
import           Rbsc.Syntax.Expr.Untyped
import qualified Rbsc.Syntax.Operators    as Ops


-- | Parser for 'Expr's.
expr :: Parser (Loc Expr)
expr = makeExprParser term table


term :: Parser (Loc Expr)
term = quantified <|> do
    e <- atom
    accessors <- many index
    return (apply accessors e)
  where
    apply = foldr (.) id . reverse


atom :: Parser (Loc Expr)
atom = choice
    [ parens expr
    , litBool
    , litNumber
    , array
    , variable
    ]


index :: Parser (Loc Expr -> Loc Expr)
index = do
    idx <- brackets expr
    return (\e -> Loc (Index e idx) (getLoc e <> getLoc idx))


litBool :: Parser (Loc Expr)
litBool = choice
    [ Loc (LitBool True)  <$> reserved "true"
    , Loc (LitBool False) <$> reserved "false"
    ]


litNumber :: Parser (Loc Expr)
litNumber = do
    n <- number
    return $ case n of
        Left d  -> LitDouble <$> d
        Right i -> LitInt <$> i


array :: Parser (Loc Expr)
array = do
    start <- symbol "{"
    e  <- expr
    es <- many (comma *> expr)
    end <- symbol "}"
    return (Loc (Array (e :| es)) (start <> end))


variable :: Parser (Loc Expr)
variable = fmap Variable <$> identifier


quantified :: Parser (Loc Expr)
quantified = do
    Loc q start <- quantifier
    name <- unLoc <$> identifier
    tyName <- optional (colon *> identifier)
    e@(Loc _ end) <- dot *> expr

    return (Loc (Quantified q name tyName e) (start <> end))
  where
    quantifier = choice
        [ Loc Ops.Forall <$> reserved "forall"
        , Loc Ops.Exists <$> reserved "exists"
        ]


-- | Operators working on 'Expr's.
type ExprOp m = Operator (ParsecT Dec Text (StateT ParserState m)) (Loc Expr)


-- | Operator table for expressions.
table :: Monad m => [[ExprOp m]]
table =
    [ [ unary Negate "-"
      ]
    , [ binary InfixN BoundTo (reserved "boundto")
      , binary InfixN Element (reserved "in")
      , hasType
      ]
    , [ binaryLOp "*" (ArithOp Ops.Mul)
      , binaryLOp "/" Divide
      ]
    , [ binaryLOp "+" (ArithOp Ops.Add)
      , binaryLOp "-" (ArithOp Ops.Sub)
      ]
    , [ binaryNOp ">"  (RelOp Ops.Gt)
      , binaryNOp ">=" (RelOp Ops.Gte)
      , binaryNOp "<"  (RelOp Ops.Lt)
      , binaryNOp "<=" (RelOp Ops.Lte)
      ]
    , [ binaryNOp "="  (EqOp Ops.Eq)
      , binaryNOp "!=" (EqOp Ops.NEq)
      ]
    , [ unary Not "!"
      ]
    , [ binaryLOp "&"  (LogicOp Ops.And)
      ]
    , [ binaryLOp "|"  (LogicOp Ops.Or)
      ]
    , [ binaryLOp "=>" (LogicOp Ops.Implies)
      ]
    ]


binaryLOp :: Monad m => String -> (Loc Expr -> Loc Expr -> Expr) -> ExprOp m
binaryLOp n c = binary InfixL c (op n)


binaryNOp :: Monad m => String -> (Loc Expr -> Loc Expr -> Expr) -> ExprOp m
binaryNOp n c = binary InfixN c (op n)


hasType :: Monad m => ExprOp m
hasType =
    Postfix $ do
        void (op ":")
        tyName <- identifier
        return (\e -> Loc (HasType e tyName) (getLoc e <> getLoc tyName))


-- | @binary assoc c p@ creates a binary infix 'Operator' with associativity
-- @assoc@ that applies the operator parser @p@. The parsed left and right
-- operands are passed to @c@ to construct the combined expression. The
-- expression is annotated with the 'Region' including both the left and
-- right operand.
binary ::
       Functor f
    => (f (Loc a -> Loc a -> Loc a) -> t)
    -> (Loc a -> Loc a -> a)
    -> f b
    -> t
binary assoc c p = assoc ((\l r -> Loc (c l r) (getLoc l <> getLoc r)) <$ p)


-- | @unary c s@ creates a prefix 'Operator' that parses the symbol @s@.
-- The expression is annotated with the 'Region' including both the operand
-- and the prefix operator.
unary :: Monad m => (Loc Expr -> Expr) -> String -> ExprOp m
unary c s =
    Prefix $ do
        rgn <- symbol s
        return (\e -> Loc (c e) (rgn <> getLoc e))


-- | Parser for an operator.
op :: String -> Parser Region
op n = fmap getLoc . lexeme . try $
    (Loc <$> string n) <* notFollowedBy punctuationChar
