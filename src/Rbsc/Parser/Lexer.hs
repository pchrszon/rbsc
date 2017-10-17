{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}


-- | Parsers for tokens.
module Rbsc.Parser.Lexer
    ( Parser
    , ParserT
    , run

    , ParserState(..)
    , currentSource
    , sources
    , initialState

    , SourceMap

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
import Control.Lens
import Control.Monad       (void)
import Control.Monad.State

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.String
import           Data.Text       (Text)

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer

import           Rbsc.Report.Region (Loc (..), Region)
import qualified Rbsc.Report.Region as Region


-- | Stores the contents of each encountered source file.
type SourceMap = Map FilePath Text


-- | The parser monad.
type Parser a = forall m. Monad m => ParserT m a


-- | Parser monad transformer with stream type 'Text'.
type ParserT m a = ParsecT Dec Text (StateT ParserState m) a


-- | @run p path content@ runs a parser @p@ on the given @content@ obtained
-- from a file with name @path@. The @path@ is only used in error messages
-- and may be empty.
run :: Monad m
    => ParserT m a
    -> FilePath
    -> Text
    -> m (Either (ParseError Char Dec) a, SourceMap)
run p path content = do
    (result, parserState) <-
        runStateT (runParserT p path content) (initialState path content)
    return (result, _sources parserState)


-- | The @ParserState@ keeps track of source file contents so that they can
-- be referenced by errors or warnings.
data ParserState = ParserState
    { _currentSource :: !Text
    , _sources       :: !SourceMap
    }


-- | Create an initial 'ParserState' from the content and path of the
-- top-level source file.
initialState :: FilePath -> Text -> ParserState
initialState path source = ParserState source (Map.singleton path source)


makeLenses ''ParserState


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
reserved :: String -> Parser Region
reserved s =
    getLoc <$> lexeme ((Loc <$> string s) <* notFollowedBy alphaNumChar)


-- | Parser for an identifier.
identifier :: IsString a => Parser (Loc a)
identifier = lexeme . try $ do
    ident <- (:) <$> letterChar <*> many alphaNumChar
    if ident `elem` reservedWords
        then fail ("unexpected reserved word " ++ ident)
        else return (Loc (fromString ident))


-- | Parser for surrounding parentheses.
parens :: Monad m => ParserT m a -> ParserT m a
parens = between (symbol "(") (symbol ")")


-- | Parser for a string literal (in double quotes).
stringLiteral :: IsString a => Parser (Loc a)
stringLiteral = lexeme $
    Loc . fromString <$> (char '"' *> Lexer.charLiteral `manyTill` char '"')


-- | Parser for a comma.
comma :: Parser Region
comma = symbol ","


-- | Parser for a semicolon.
semi :: Parser Region
semi = symbol ";"


-- | Parser for a symbol.
symbol :: String -> Parser Region
symbol s = getLoc <$> lexeme (Loc <$> string s)


-- | Annotate a parsed value with its 'Region' in the source and skip
-- trailing white space.
lexeme :: Monad m => ParserT m (Region -> a) -> ParserT m a
lexeme p = do
    source <- use currentSource
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
