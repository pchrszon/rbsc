module Rbsc.Parser
    ( parse
    ) where


import Control.Lens
import Control.Monad.State

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.String
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text

import System.Directory
import System.FilePath

import Text.Megaparsec       hiding (parse)
import Text.Megaparsec.Error (parseErrorTextPretty)

import           Rbsc.Parser.Declaration
import           Rbsc.Parser.Lexer
import           Rbsc.Parser.TypeLevel
import qualified Rbsc.Report.Error.Syntax as Syntax
import qualified Rbsc.Report.Region       as Region
import           Rbsc.Syntax.Declaration


-- | Parse a source file.
parse :: MonadIO m => FilePath -> m (Either [Syntax.Error] [Declaration])
parse path = do
    content <- liftIO (Text.readFile path)
    (result, sourceMap) <- run modelFile path content

    return $ case result of
        Left err -> Left [fromParseError sourceMap err]
        Right errorOrDecls ->
            let errors = toListOf (traverse._Left) errorOrDecls
                decls  = toListOf (traverse._Right) errorOrDecls
            in if null errors
                   then Right decls
                   else Left (fmap (fromParseError sourceMap) errors)


modelFile :: MonadIO m => ParserT m [ErrorOrDecl]
modelFile =
    concat <$> between sc eof (many (include <|> fmap (: []) declaration))


declaration :: Parser ErrorOrDecl
declaration = choice
    [ declType
    ]


include :: MonadIO m => ParserT m [ErrorOrDecl]
include = do
    void (reserved "include")
    includePath <- Region.unAnn <$> stringLiteral

    -- includePath is relative to file containing the include keyword,
    -- thus we need to make the path relative to our current working
    -- directory
    parentPath <- sourceName <$> getPosition
    let parentDir = dropFileName parentPath
        path = parentDir </> includePath

    exists <- liftIO (doesFileExist path)
    if not exists
        then fail ("file " ++ path ++ " does not exist")
        else parseIncludeFile path


parseIncludeFile :: MonadIO m => FilePath -> ParserT m [ErrorOrDecl]
parseIncludeFile path = do
    content <- liftIO (Text.readFile path)
    sources.at path .= Just content

    -- save current parser state
    input <- getInput
    source <- use currentSource

    -- switch to include file
    pushPosition (initialPos path)
    setInput content
    currentSource .= content

    -- parse include file
    result <- modelFile

    -- switch back to current file
    currentSource .= source
    setInput input
    popPosition

    return result


fromParseError ::
       (Ord t, ShowToken t, ShowErrorComponent e)
    => SourceMap
    -> ParseError t e
    -> Syntax.Error
fromParseError sourceMap err = Syntax.ParseError rgn msg
  where
    rgn = Region.Region path content start end
    msg = fromString (parseErrorTextPretty err)

    path    = sourceName pos
    content = fromMaybe Text.empty (Map.lookup path sourceMap)

    start = fromSourcePos pos
    end   = start { Region.column = Region.column start + 1 }
    pos   = NonEmpty.head (errorPos err)
