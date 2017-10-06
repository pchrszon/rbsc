module Rbsc.Parser
    ( modelFile
    ) where


import Control.Monad.IO.Class

import qualified Data.Text.IO as TIO

import System.Directory

import Text.Megaparsec

import Rbsc.Parser.Lexer
import Rbsc.Parser.TypeLevel
import Rbsc.SourceSpan (SourceSpan)
import Rbsc.Syntax.Declaration


modelFile :: MonadIO m => ParserT m [Declaration SourceSpan]
modelFile =
    concat <$> between sc eof (many (include <|> fmap (: []) declaration))


declaration :: ParserT m (Declaration SourceSpan)
declaration = choice
    [ declType
    ]


include :: MonadIO m => ParserT m [Declaration SourceSpan]
include = do
    reserved "include"
    path <- stringLiteral

    exists <- liftIO (doesFileExist path)
    if not exists
        then fail ("file " ++ path ++ " does not exist")
        else parseIncludeFile path


parseIncludeFile :: MonadIO m => FilePath -> ParserT m [Declaration SourceSpan]
parseIncludeFile path = do
    contents <- liftIO (TIO.readFile path)

    input <- getInput
    pushPosition (initialPos path)
    setInput contents

    result <- modelFile

    setInput input
    popPosition

    return result
