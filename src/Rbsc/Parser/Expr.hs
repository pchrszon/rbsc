{-# LANGUAGE RankNTypes #-}


-- | Parser for expressions.
module Rbsc.Parser.Expr
    ( expr
    ) where


import Control.Monad
import Control.Monad.State

import Data.Semigroup
import Data.Text      (Text)

import Text.Megaparsec
import Text.Megaparsec.Expr

import           Rbsc.Parser.Lexer
import           Rbsc.Syntax.Expr.Untyped
import qualified Rbsc.Syntax.Operators    as Ops


-- | Parser for 'Expr's.
expr :: Parser (Loc Expr)
expr = makeExprParser atom table


atom :: Parser (Loc Expr)
atom = choice
    [ parens expr
    , litBool
    , quantified
    , variable
    ]


litBool :: Parser (Loc Expr)
litBool = choice
    [ Loc (LitBool True)  <$> reserved "true"
    , Loc (LitBool False) <$> reserved "false"
    ]


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


variable :: Parser (Loc Expr)
variable = fmap Variable <$> identifier


-- | Operators working on 'Expr's.
type ExprOp m = Operator (ParsecT Dec Text (StateT ParserState m)) (Loc Expr)


-- | Operator table for expressions.
table :: Monad m => [[ExprOp m]]
table =
    [ [ boolNot ]
    , [ binary InfixN BoundTo (reserved "boundto")
      , binary InfixN Element (reserved "in")
      , hasType
      ]
    , [ boolBinOp "&" Ops.And
      , boolBinOp "|" Ops.Or
      ]
    , [ boolBinOp "->" Ops.Implies ]
    ]


boolBinOp :: Monad m => String -> Ops.BoolBinOp -> ExprOp m
boolBinOp n binOp = binary InfixL (BoolBinOp binOp) (op n)


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


boolNot :: Monad m => ExprOp m
boolNot =
    Prefix $ do
        rgn <- symbol "!"
        return (\e -> Loc (Not e) (rgn <> getLoc e))


-- | Parser for an operator.
op :: String -> Parser Region
op n = fmap getLoc . lexeme . try $
    (Loc <$> string n) <* notFollowedBy punctuationChar
