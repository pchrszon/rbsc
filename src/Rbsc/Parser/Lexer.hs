{-# LANGUAGE TypeFamilies #-}


-- | Parsers for tokens.
module Rbsc.Parser.Lexer
    ( ParserT
    , reservedWords
    , reserved
    , identifier
    , parens
    , comma
    , semi
    , symbol
    , lexeme
    , sc
    , loc
    ) where


import Control.Applicative
import Control.Monad       (void)

import Data.String
import Data.Text   (Text)

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

import qualified Rbsc.SourceSpan as S


-- | Parser monad transformer with stream type 'Text'.
type ParserT m a = ParsecT Dec Text m a


-- | List of reserved keywords.
reservedWords :: [String]
reservedWords =
    [ "natural"
    , "role"
    , "compartment"
    , "type"
    ]


-- | Parser for a reserved word.
reserved :: String -> ParserT m ()
reserved s = lexeme (string s *> notFollowedBy alphaNumChar)


-- | Parser for an identifier.
identifier :: IsString a => ParserT m a
identifier = lexeme . try $ do
    ident <- (:) <$> letterChar <*> many alphaNumChar
    if ident `elem` reservedWords
        then fail ("unexpected reserved word " ++ ident)
        else return (fromString ident)


-- | Parser for surrounding parentheses-
parens :: ParserT m a -> ParserT m a
parens = between (symbol "(") (symbol ")")


-- | Parser for a comma.
comma :: ParserT m ()
comma = void (symbol ",")


-- | Parser for a semicolon.
semi :: ParserT m ()
semi = void (symbol ";")


-- | Parser for a symbol.
symbol :: String -> ParserT m String
symbol = L.symbol sc


-- | Parse the given lexeme and skip any following white space.
lexeme :: ParserT m a -> ParserT m a
lexeme = L.lexeme sc


-- | Parser for non-empty white space (including newlines).
sc :: ParserT m ()
sc = L.space (void spaceChar) (L.skipLineComment "//") empty


-- | Annotate a parsed value with its 'SourceSpan' in the source.
loc :: ParserT m (S.SourceSpan -> a) -> ParserT m a
loc p = do
    from <- convert <$> getPosition
    x <- p
    to <- convert <$> getPosition
    return (x (S.SourceSpan from to))
  where
    convert (SourcePos _ line col) = S.SourcePos (fromPos line) (fromPos col)
    fromPos = fromIntegral . unPos
