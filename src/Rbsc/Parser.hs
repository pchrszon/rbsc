module Rbsc.Parser
    ( parse
    ) where


import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader

import qualified Data.List.NonEmpty as NonEmpty
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Error (parseErrorTextPretty)

import Rbsc.Parser.Declaration
import Rbsc.Parser.Lexer
import Rbsc.Parser.TypeLevel
import qualified Rbsc.Report.Error.Syntax as Syntax
import qualified Rbsc.Report.Region as Region
import Rbsc.Syntax.Declaration


-- | Parse a source file.
parse :: MonadIO m => FilePath -> m (Either [Syntax.Error] [Declaration])
parse path = do
    content <- liftIO (TIO.readFile path)
    result <- runParserT (runReaderT modelFile content) path content
    case result of
        Left err -> Left . (: []) <$> fromParseError err
        Right errorOrDecls ->
            let errors = toListOf (traverse._Left) errorOrDecls
                decls  = toListOf (traverse._Right) errorOrDecls
            in if null errors
                   then return (Right decls)
                   else Left <$> traverse fromParseError errors


modelFile :: MonadIO m => ParserT m [ErrorOrDecl]
modelFile =
    concat <$> between sc eof (many (include <|> fmap (: []) declaration))


declaration :: ParserT m ErrorOrDecl
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
    content <- liftIO (TIO.readFile path)

    input <- getInput
    pushPosition (initialPos path)
    setInput content

    -- set 'contents' as new source
    result <- local (const content) modelFile

    setInput input
    popPosition

    return result


fromParseError ::
       (Ord t, ShowToken t, ShowErrorComponent e, MonadIO m)
    => ParseError t e
    -> m Syntax.Error
fromParseError err = do
    exists <- liftIO (doesFileExist path)
    content <-
        if exists
            then liftIO (TIO.readFile path)
            else return Text.empty

    let rgn = Region.Region path content start end
        msg = fromString (parseErrorTextPretty err)

    return (Syntax.ParseError rgn msg)
  where
    pos   = NonEmpty.head (errorPos err)
    start = fromSourcePos pos
    end   = start { Region.column = Region.column start + 1 }
    path = sourceName pos
