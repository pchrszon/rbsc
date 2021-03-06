{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


-- | Parser for expressions.
module Rbsc.Parser.Expr
    ( expr
    , range
    , quantifiedType
    , componentTypeSet
    ) where


import Control.Applicative            hiding (many)
import Control.Monad.Combinators.Expr
import Control.Monad.State.Strict

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set           as Set
import           Data.Text          (Text)
import           Data.Void

import Text.Megaparsec


import Rbsc.Data.ComponentType
import Rbsc.Data.Function      (FunctionName (..))

import Rbsc.Parser.Lexer

import qualified Rbsc.Syntax.Operators      as Ops
import           Rbsc.Syntax.Quantification
import           Rbsc.Syntax.Untyped.Expr


-- | Parser for ranges.
range :: Parser (LExpr, LExpr)
range = brackets ((,) <$> expr <*> (operator ".." *> expr))


-- | Parser for 'Expr's.
expr :: Parser LExpr
expr = makeExprParser term table <?> "expression"


componentTypeSet :: Parser (Loc ComponentTypeSet)
componentTypeSet = choice
    [ Loc AllComponents   <$> reserved "component"
    , Loc AllNaturals     <$> reserved "natural"
    , Loc AllRoles        <$> reserved "role"
    , Loc AllCompartments <$> reserved "compartment"
    , singleType
    , typeSet
    ]
  where
    singleType = do
        tyIdent@(Loc _ rgn) <- identifier
        return (Loc (ComponentTypeSet (Set.singleton tyIdent)) rgn)
    typeSet = do
        tyIdents <- braces (identifier `sepBy1` comma)
        let rgn = foldr1 (<>) (fmap getLoc tyIdents)
        return (Loc (ComponentTypeSet (Set.fromList tyIdents)) rgn)


term :: Parser LExpr
term = label "expression" $ quantified <|> do
    e <- atom
    postfix <- many (member <|> index <|> call)
    return (apply postfix e)
  where
    apply = foldr (.) id . reverse


atom :: Parser LExpr
atom = choice
    [ parens expr
    , litBool
    , litNumber
    , litAction
    , litArray
    , constraint
    , ifThenElse
    , function
    , countFunction
    , lengthFunction
    , playerFunction
    , playableFunction
    , indexFunction
    , self
    , ident
    ]


member :: Parser (LExpr -> LExpr)
member = label "operator" $ do
    _ <- operator "."
    Loc name rgn <- identifier
    return (\e -> Loc (Member e name) (getLoc e <> rgn))


index :: Parser (LExpr -> LExpr)
index = label "operator" $ do
    _ <- symbol "["
    idx <- expr
    end <- symbol "]"
    return (\e -> Loc (Index e idx) (getLoc e <> end))


call :: Parser (LExpr -> LExpr)
call = label "operator" $ do
    _ <- symbol "("
    args <- expr `sepBy` comma
    end <- symbol ")"
    return (\e -> Loc (Call e args) (getLoc e <> end))


litBool :: Parser LExpr
litBool = choice
    [ Loc (LitBool True)  <$> reserved "true"
    , Loc (LitBool False) <$> reserved "false"
    ]


litNumber :: Parser LExpr
litNumber = do
    n <- number
    return $ case n of
        Left d  -> LitDouble <$> d
        Right i -> LitInt . fromInteger <$> i


litAction :: Parser LExpr
litAction = do
    start <- reserved "action"
    e <- expr
    return (Loc (LitAction e) (start <> getLoc e))


constraint :: Parser LExpr
constraint = do
    start <- reserved "constraint"
    e <- expr
    return (Loc (Constraint e) (start <> getLoc e))


litArray :: Parser LExpr
litArray = do
    start <- symbol "["
    e <- expr
    optional (operator "||") >>= \case
        Just _ -> do
            var <- unLoc <$> identifier
            _ <- colon
            (lower, upper) <- range
            end <- symbol "]"
            return (Loc (GenArray e var lower upper) (start <> end))
        Nothing -> do
            es <- many (comma *> expr)
            end <- symbol "]"
            return (Loc (LitArray (e :| es)) (start <> end))


ifThenElse :: Parser LExpr
ifThenElse = mkIfThenElse
    <$> reserved "if"
    <*> expr
    <*> (reserved "then" *> expr)
    <*> (reserved "else" *> expr)
  where
    mkIfThenElse start c t e = Loc (IfThenElse c t e) (start <> getLoc e)


function :: Parser LExpr
function = choice
    [ fn FuncMinDouble "minf"
    , fn FuncMinInt    "min"
    , fn FuncMaxDouble "maxf"
    , fn FuncMaxInt    "max"
    , fn FuncFloor     "floor"
    , fn FuncCeil      "ceil"
    , fn FuncPowInt    "pow"
    , fn FuncPowDouble "powf"
    , fn FuncMod       "mod"
    , fn FuncLog       "log"
    ]
  where
    fn sym name = do
        rgn <- reserved name
        return (Loc (LitFunction sym) rgn)


countFunction :: Parser LExpr
countFunction = specialFunction "count"
    (Count <$> (unLoc <$> componentTypeSet) <*> (comma *> expr))


lengthFunction :: Parser LExpr
lengthFunction = specialFunction "length" (Length <$> expr)


playerFunction :: Parser LExpr
playerFunction = keywordFunction "player" Player


playableFunction :: Parser LExpr
playableFunction = specialFunction "playable" $ Playable
    <$> expr
    <*> optional (comma *> expr)


indexFunction :: Parser LExpr
indexFunction = keywordFunction "index" ComponentIndex


specialFunction :: Text -> Parser Expr -> Parser LExpr
specialFunction name p = do
    start <- reserved name
    _ <- symbol "("
    e <- p
    end <- symbol ")"
    return (Loc e (start <> end))


keywordFunction :: Text -> (LExpr -> Expr) -> Parser LExpr
keywordFunction name con = do
    start <- reserved name
    arg <- optional ((,) <$> (symbol "(" *> expr) <*> symbol ")")
    case arg of
        Just (e, end) -> return (Loc (con e) (start <> end))
        Nothing       -> return (Loc (con (Loc Self start)) start)


self :: Parser LExpr
self = Loc Self <$> reserved "self"


ident :: Parser LExpr
ident = fmap Identifier <$> identifier


quantified :: Parser LExpr
quantified = do
    Loc q start <- quantifier
    ((name, tySet) : vars) <- variable `sepBy1` comma
    e@(Loc _ end) <- dot *> expr

    let e' = apply (fmap (mkQuantified q end) vars) e
    return (Loc (Quantified q (unLoc name) tySet e') (start <> end))
  where
    mkQuantified q end (Loc name start, tySet) e =
        Loc (Quantified q name tySet e) (start <> end)

    variable = (,)
        <$> (identifier <?> "variable")
        <*> option (QdTypeComponent AllComponents) (colon *> quantifiedType)

    quantifier = label "quantifier" $ choice
        [ Loc Forall  <$> reserved "forall"
        , Loc Exists  <$> reserved "exists"
        , Loc Sum     <$> reserved "sum"
        , Loc Product <$> reserved "product"
        ]

    apply = foldr (.) id


quantifiedType :: Parser (QuantifiedType ComponentTypeSet LExpr)
quantifiedType = label "type" $
    QdTypeInt <$> range <|> QdTypeComponent <$> (unLoc <$> componentTypeSet)


-- | Operators working on 'Expr's.
type ExprOp m = Operator (ParsecT Void Text (StateT ParserState m)) LExpr


-- | Operator table for expressions.
table :: Monad m => [[ExprOp m]]
table =
    [ [ unary Negate "-"
      ]
    , [ binary InfixN BoundTo (reserved "boundto" <?> "operator")
      , binary InfixN Element (reserved "in" <?> "operator")
      , infixFunction
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


binaryLOp :: Monad m => Text -> (Loc Expr -> Loc Expr -> Expr) -> ExprOp m
binaryLOp n c = binary InfixL c (operator n <?> "operator")


binaryNOp :: Monad m => Text -> (Loc Expr -> Loc Expr -> Expr) -> ExprOp m
binaryNOp n c = binary InfixN c (operator n <?> "operator")


hasType :: Monad m => ExprOp m
hasType =
    Postfix . try $ do
        -- Fail if there is an assignment following. In that case, the
        -- colon denotes a probability, not a @HasType@ relation.
        _ <- operator ":" <* notFollowedBy (symbol "(") <?> "operator"
        Loc tySet rgn <- componentTypeSet
        return (\e -> Loc (HasType e tySet) (getLoc e <> rgn))


infixFunction :: Monad m => ExprOp m
infixFunction =
    InfixN $ do
        funcIdent <- fmap Identifier <$> identifier <?> "infix function"
        return (\l r -> Loc (Call funcIdent [l, r]) (getLoc l <> getLoc r))


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
unary :: Monad m => (Loc Expr -> Expr) -> Text -> ExprOp m
unary c s =
    Prefix $ do
        rgn <- operator s <?> "operator"
        return (\e -> Loc (c e) (rgn <> getLoc e))
