{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}


-- | Parsers for tokens.
module Rbsc.Parser.Lexer
    ( -- * Parser monad
      Parser
    , ParserT
    , run
    , testRun

    , ParserState(..)
    , currentSource
    , sources
    , initialState

    , SourceMap

      -- * Token parsers
    , reserved
    , operator
    , identifier
    , block
    , parens
    , braces
    , brackets
    , commaSepNonEmpty
    , stringLiteral
    , dot
    , comma
    , semi
    , colon
    , equals
    , integer
    , number
    , symbol
    , lexeme
    , sc

      -- * Parse error recovery
    , withRecoveryOn

      -- * Source location annotation
    , Loc(..)
    , Region
    , fromSourcePos
    ) where


import Control.Applicative
import Control.Lens
import Control.Monad              (void)
import Control.Monad.State.Strict

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.String
import           Data.Text          (Text)

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as Lexer


import           Rbsc.Report.Region (Loc (..), Region)
import qualified Rbsc.Report.Region as Region

import Rbsc.Parser.Reserved


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


-- | @testRun@ applies a parser to a given input and shows the results.
testRun :: Show a => ParserT IO a -> Text -> IO (Either String a)
testRun p content = do
    (result, _) <- run p "" content
    return (over _Left parseErrorPretty result)


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


-- | Parser for a reserved word.
reserved :: String -> Parser Region
reserved s =
    getLoc <$> lexeme ((Loc <$> string s) <* notFollowedBy alphaNumChar)


-- | Parser for an operator.
--
-- This parser checks that the parsed operator is not a prefix of another
-- valid operator.
operator :: String -> Parser Region
operator s = getLoc <$>
    try (lexeme ((Loc <$> string s) <* notFollowedBy (oneOf opLetter)))


-- | Parser for an identifier.
identifier :: IsString a => Parser (Loc a)
identifier = lexeme . try $ do
    ident <- (:) <$> identStart <*> many identLetter
    if ident `elem` reservedWords
        then fail ("unexpected reserved word " ++ ident)
        else return (Loc (fromString ident))
  where
    identStart  = letterChar <|> char '_'
    identLetter = alphaNumChar <|> char '_'


-- | @block name p@ is a parser for named blocks. A block starts with
-- @name@ followed by @p@ surrounded by braces.
block :: Monad m => String -> ParserT m a -> ParserT m a
block name p = reserved name *> braces p


-- | Parser for surrounding parentheses.
parens :: Monad m => ParserT m a -> ParserT m a
parens = between (symbol "(") (symbol ")")


-- | Parser for surrounding braces.
braces :: Monad m => ParserT m a -> ParserT m a
braces = between (symbol "{") (symbol "}")


-- | Parser for surrounding brackets.
brackets :: Monad m => ParserT m a -> ParserT m a
brackets = between (symbol "[") (symbol "]")


-- | @commaSepNonEmpty p@ parses a comma separated list of @p@s.
commaSepNonEmpty :: Monad m => ParserT m a -> ParserT m (NonEmpty a)
commaSepNonEmpty p = do
    x <- p
    xs <- many (comma *> p)
    return (x :| xs)


-- | Parser for a string literal (in double quotes).
stringLiteral :: IsString a => Parser (Loc a)
stringLiteral = lexeme $
    Loc . fromString <$> (char '"' *> Lexer.charLiteral `manyTill` char '"')


-- | Parser for a dot.
dot :: Parser Region
dot = symbol "."


-- | Parser for a comma.
comma :: Parser Region
comma = symbol ","


-- | Parser for a semicolon.
semi :: Parser Region
semi = symbol ";"


-- | Parser for a colon.
colon :: Parser Region
colon = symbol ":"


-- | Parser for an equals symbol.
equals :: Parser Region
equals = symbol "="


-- | Parser for an integer.
integer :: Parser (Loc Integer)
integer = lexeme (Loc <$> Lexer.signed sc Lexer.integer)


-- | Parser for decimal numbers.
number :: Parser (Either (Loc Double) (Loc Integer))
number = (Left <$> try float) <|> (Right <$> integer)


float :: Parser (Loc Double)
float = lexeme (Loc <$> Lexer.signed sc Lexer.float)


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


-- | @withRecoveryOn end p@ runs parser @p@. In case @p@ fails, the input
-- is skipped until @end@ is parsed successfully.
withRecoveryOn ::
       Monad m
    => ParserT m b
    -> ParserT m a
    -> ParserT m (Either (ParseError Char Dec) a)
withRecoveryOn end =
    withRecovery (\err -> Left err <$ anyChar `manyTill` end) . fmap Right


-- | Convert a 'SourcePos' to a 'Region.Position'.
fromSourcePos :: SourcePos -> Region.Position
fromSourcePos (SourcePos _ line col) =
        Region.Position (fromPos line) (fromPos col)
  where
    fromPos = fromIntegral . unPos
