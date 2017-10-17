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
    , fromSourcePos
    ) where


import Control.Applicative
import Control.Monad       (void)
import Control.Monad.Reader

import Data.String
import Data.Text   (Text)

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer

import Rbsc.Report.Region (Region, Ann(..))
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
reserved :: String -> ParserT m Region
reserved s =
    getAnn <$> lexeme ((Ann <$> string s) <* notFollowedBy alphaNumChar)


-- | Parser for an identifier.
identifier :: IsString a => ParserT m (Ann a Region)
identifier = lexeme . try $ do
    ident <- (:) <$> letterChar <*> many alphaNumChar
    if ident `elem` reservedWords
        then fail ("unexpected reserved word " ++ ident)
        else return (Ann (fromString ident))


-- | Parser for surrounding parentheses.
parens :: ParserT m a -> ParserT m a
parens = between (symbol "(") (symbol ")")


-- | Parser for a string literal (in double quotes).
stringLiteral :: IsString a => ParserT m (Ann a Region)
stringLiteral = lexeme $
    Ann . fromString <$> (char '"' *> Lexer.charLiteral `manyTill` char '"')


-- | Parser for a comma.
comma :: ParserT m Region
comma = symbol ","


-- | Parser for a semicolon.
semi :: ParserT m Region
semi = symbol ";"


-- | Parser for a symbol.
symbol :: String -> ParserT m Region
symbol s = getAnn <$> lexeme (Ann <$> string s)


-- | Annotate a parsed value with its 'Region' in the source and skip
-- trailing white space.
lexeme :: ParserT m (Region -> a) -> ParserT m a
lexeme p = do
    source <- ask
    start <- getPosition
    f <- p
    end <- getPosition
    sc

    let rgn = Region.Region
            (sourceName start) source (fromSourcePos start) (fromSourcePos end)

    return (f rgn)


-- | Parser for non-empty white space (including newlines).
sc :: ParserT m ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "//") empty


-- | Convert a 'SourcePos' to a 'Region.Position'.
fromSourcePos :: SourcePos -> Region.Position
fromSourcePos (SourcePos _ line col) =
        Region.Position (fromPos line) (fromPos col)
  where
    fromPos = fromIntegral . unPos
