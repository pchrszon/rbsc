module Rbsc.Parser
    ( modelFile
    ) where


import Control.Monad.IO.Class

import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath

import Text.Megaparsec

import Rbsc.Parser.Declaration
import Rbsc.Parser.Lexer
import Rbsc.Parser.TypeLevel


modelFile :: MonadIO m => ParserT m [ErrorOrDecl]
modelFile =
    concat <$> between sc eof (many (include <|> fmap (: []) declaration))


declaration :: ParserT m ErrorOrDecl
declaration = choice
    [ declType
    ]


include :: MonadIO m => ParserT m [ErrorOrDecl]
include = do
    reserved "include"
    includePath <- stringLiteral

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
    contents <- liftIO (TIO.readFile path)

    input <- getInput
    pushPosition (initialPos path)
    setInput contents

    result <- modelFile

    setInput input
    popPosition

    return result
