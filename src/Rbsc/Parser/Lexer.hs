{-# LANGUAGE TypeFamilies #-}


-- | Parsers for tokens.
module Rbsc.Parser.Lexer
    ( ParserT
    , reservedWords
    , reserved
    , identifier
    , parens
    , stringLiteral
    , comma
    , semi
    , symbol
    , lexeme
    , sc
    , loc
    ) where


import Control.Applicative
import Control.Monad       (void)
import Control.Monad.Reader

import Data.String
import Data.Text   (Text)

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer

import qualified Rbsc.Report.Region as Region


-- | Parser monad transformer with stream type 'Text'.
type ParserT m a = ReaderT Text (ParsecT Dec Text m) a


-- | List of reserved keywords.
reservedWords :: [String]
reservedWords =
    [ "include"
    , "natural"
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


-- | Parser for a string literal (in double quotes).
stringLiteral :: IsString a => ParserT m a
stringLiteral =
    fromString <$> (char '"' *> Lexer.charLiteral `manyTill` char '"')


-- | Parser for a comma.
comma :: ParserT m ()
comma = void (symbol ",")


-- | Parser for a semicolon.
semi :: ParserT m ()
semi = void (symbol ";")


-- | Parser for a symbol.
symbol :: String -> ParserT m String
symbol = Lexer.symbol sc


-- | Parse the given lexeme and skip any following white space.
lexeme :: ParserT m a -> ParserT m a
lexeme = Lexer.lexeme sc


-- | Parser for non-empty white space (including newlines).
sc :: ParserT m ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "//") empty


-- | Annotate a parsed value with its 'Region' in the source.
loc :: ParserT m (Region.Region -> a) -> ParserT m a
loc p = do
    source <- ask
    start <- getPosition
    f <- p
    end <- getPosition

    let rgn = Region.Region
            (sourceName start) source (convert start) (convert end)

    return (f rgn)
  where
    convert (SourcePos _ line col) =
        Region.Position (fromPos line) (fromPos col)
    fromPos = fromIntegral . unPos
