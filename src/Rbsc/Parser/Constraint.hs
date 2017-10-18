{-# LANGUAGE RankNTypes #-}


-- | Parser for constraint expressions.
module Rbsc.Parser.Constraint
    ( constraint
    ) where


import Control.Monad
import Control.Monad.State

import Data.Semigroup
import Data.Text      (Text)

import Text.Megaparsec
import Text.Megaparsec.Expr

import           Rbsc.Parser.Lexer
import           Rbsc.Syntax.Constraint
import qualified Rbsc.Syntax.Operators  as Ops


-- | Parser for 'Constraint's.
constraint :: Parser (Loc Constraint)
constraint = makeExprParser atom table


atom :: Parser (Loc Constraint)
atom = choice
    [ parens constraint
    , litBool
    , quantified
    , variable
    ]


litBool :: Parser (Loc Constraint)
litBool = choice
    [ Loc (LitBool True)  <$> reserved "true"
    , Loc (LitBool False) <$> reserved "false"
    ]


quantified :: Parser (Loc Constraint)
quantified = do
    Loc q start <- quantifier
    name <- unLoc <$> identifier
    tyName <- optional (colon *> (unLoc <$> identifier))
    c@(Loc _ end) <- dot *> constraint

    return (Loc (Quantified q name tyName c) (start <> end))
  where
    quantifier = choice
        [ Loc Ops.Forall <$> reserved "forall"
        , Loc Ops.Exists <$> reserved "exists"
        ]


variable :: Parser (Loc Constraint)
variable = fmap Variable <$> identifier


-- | Operators working on 'Constraint's.
type ConstraintOp m
     = Operator (ParsecT Dec Text (StateT ParserState m)) (Loc Constraint)


-- | Operator table for expressions.
table :: Monad m => [[ConstraintOp m]]
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


boolBinOp :: Monad m => String -> Ops.BoolBinOp -> ConstraintOp m
boolBinOp n binOp = binary InfixL (BoolBinOp binOp) (op n)


hasType :: Monad m => ConstraintOp m
hasType =
    Postfix $ do
        void (op ":")
        Loc tyName rgn <- identifier
        return (\c -> Loc (HasType c tyName) (getLoc c <> rgn))


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


boolNot :: Monad m => ConstraintOp m
boolNot =
    Prefix $ do
        rgn <- symbol "!"
        return (\c -> Loc (Not c) (rgn <> getLoc c))


-- | Parser for an operator.
op :: String -> Parser Region
op n = fmap getLoc . lexeme . try $
    (Loc <$> string n) <* notFollowedBy punctuationChar
