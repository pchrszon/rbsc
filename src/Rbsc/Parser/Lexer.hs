{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


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
    , constArgs
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


import Control.Applicative        hiding (many)
import Control.Lens
import Control.Monad.State.Strict

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.String
import           Data.Text          (Text, pack)
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer


import Rbsc.Data.Name

import           Rbsc.Report.Error  (Error (..), LocError (..))
import qualified Rbsc.Report.Error  as Error
import           Rbsc.Report.Region (Loc (..), Region)
import qualified Rbsc.Report.Region as Region

import Rbsc.Parser.Reserved

import Rbsc.Syntax.Untyped (LExpr)


-- | Stores the contents of each encountered source file.
type SourceMap = Map FilePath Text


-- | A set of constant definitions which has been provided on the command line.
type ConstArgs = Map Name LExpr


-- | The parser monad.
type Parser a = forall m. Monad m => ParserT m a


-- | Parser monad transformer with stream type 'Text'.
type ParserT m a = ParsecT Void Text (StateT ParserState m) a


-- | @run p path content@ runs a parser @p@ on the given @content@ obtained
-- from a file with name @path@. The @path@ is only used in error messages
-- and may be empty.
run :: Monad m
    => ParserT m a
    -> FilePath
    -> Text
    -> ConstArgs
    -> m (Either (ParseErrorBundle Text Void) a, SourceMap)
run p path content consts = do
    (result, parserState) <-
        runStateT (runParserT p path content) (initialState path content consts)
    return (result, _sources parserState)


-- | @testRun@ applies a parser to a given input and shows the results.
testRun :: ParserT IO a -> Text -> IO (Either String a)
testRun p content = do
    (result, _) <- run p "" content Map.empty
    return (over _Left errorBundlePretty result)


-- | The @ParserState@ keeps track of source file contents so that they can
-- be referenced by errors or warnings.
data ParserState = ParserState
    { _currentSource :: !Text
    , _sources       :: !SourceMap
    , _constArgs     :: !ConstArgs
    }


-- | Create an initial 'ParserState' from the content and path of the
-- top-level source file.
initialState :: FilePath -> Text -> ConstArgs -> ParserState
initialState path source = ParserState source (Map.singleton path source)


makeLenses ''ParserState


-- | Parser for a reserved word.
reserved :: Text -> Parser Region
reserved s =
    getLoc <$> lexeme (try ((Loc <$> string s) <*
    notFollowedBy (alphaNumChar <|> char '_')))


-- | Parser for an operator.
--
-- This parser checks that the parsed operator is not a prefix of another
-- valid operator.
operator :: Text -> Parser Region
operator s = getLoc <$>
    try (lexeme ((Loc <$> string s) <* notFollowedBy (oneOf opLetter)))


-- | Parser for an identifier.
identifier :: IsString a => Parser (Loc a)
identifier = label "identifier" . lexeme . try $ do
    ident <- (:) <$> identStart <*> many identLetter
    if pack ident `elem` reservedWords
        then fail ("unexpected reserved word " ++ ident)
        else return (Loc (fromString ident))
  where
    identStart  = letterChar <|> char '_'
    identLetter = alphaNumChar <|> char '_'


-- | @block name p@ is a parser for named blocks. A block starts with
-- @name@ followed by @p@ surrounded by braces.
block :: Monad m => Text -> ParserT m a -> ParserT m a
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
integer = lexeme (Loc <$> Lexer.signed sc Lexer.decimal)


-- | Parser for decimal numbers.
number :: Parser (Either (Loc Double) (Loc Integer))
number = (Left <$> try float) <|> (Right <$> integer)


float :: Parser (Loc Double)
float = lexeme (Loc <$> Lexer.signed sc Lexer.float)


-- | Parser for a symbol.
symbol :: Text -> Parser Region
symbol s = getLoc <$> lexeme (Loc <$> string s)


-- | Annotate a parsed value with its 'Region' in the source and skip
-- trailing white space.
lexeme :: Monad m => ParserT m (Region -> a) -> ParserT m a
lexeme p = do
    source <- use currentSource
    start <- getSourcePos
    f <- p
    end <- getSourcePos
    sc

    let rgn = Region.Region
            (sourceName start) source (fromSourcePos start) (fromSourcePos end)

    return (f rgn)


-- | Parser for non-empty white space (including newlines).
sc :: ParserT m ()
sc = Lexer.space (void spaceChar) (Lexer.skipLineComment "//") empty


-- | @withRecoveryOn end p@ runs parser @p@. In case @p@ fails, the input
-- is skipped until @end@ is parsed successfully.
withRecoveryOn
    :: Monad m => ParserT m b -> ParserT m a -> ParserT m (Either Error a)
withRecoveryOn terminator = withRecovery recover . fmap Right
  where
    recover err = do
        pos     <- getSourcePos
        content <- use currentSource

        let path  = sourceName pos
            start = fromSourcePos pos
            end   = start { Region.column = Region.column start + 1 }
            msg   = fromString (parseErrorTextPretty err)
            rgn   = Region.Region path content start end

        _ <- anySingle `manyTill` terminator

        return (Left (LocError (MkLocError rgn (Error.ParseError msg))))


-- | Convert a 'SourcePos' to a 'Region.Position'.
fromSourcePos :: SourcePos -> Region.Position
fromSourcePos (SourcePos _ line col) =
        Region.Position (fromPos line) (fromPos col)
  where
    fromPos = fromIntegral . unPos
