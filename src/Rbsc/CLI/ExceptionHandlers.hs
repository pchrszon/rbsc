{-# LANGUAGE OverloadedStrings #-}


-- | Handlers for IO exceptions and internal errors.
module Rbsc.CLI.ExceptionHandlers
    ( ioExceptionHandler
    , errorHandler
    ) where


import Control.Lens
import Control.Exception

import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.UUID
import Data.Text (pack)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

import System.IO
import System.IO.Error.Lens
import System.Exit
import System.Environment
import System.Random


import Rbsc.Parser (SourceMap)


-- | Handles an 'IOException' by showing the error message and exiting the
-- program.
ioExceptionHandler :: IOException -> IO a
ioExceptionHandler e = do
    let file = fromMaybe "<unknown source>" (view fileName e)
    hPutStrLn stderr (view description e ++ ": " ++ file)
    exitWith (ExitFailure 2)


-- | Handle an internal error by writing an error report to a file.
errorHandler :: SourceMap -> String -> IO a
errorHandler sourceMap err = do
    hPutStrLn stderr "An internal error occurred"

    uuid <- randomIO :: IO UUID
    let reportPath = "report_" ++ show uuid ++ ".log"

    writeReport reportPath sourceMap err

    hPutStrLn stderr ("A crash report has been written to " ++ reportPath)
    exitWith (ExitFailure 4)


writeReport :: FilePath -> SourceMap -> String -> IO ()
writeReport path sourceMap err = do
    let errorMessage = "// " <> pack err <> "\n"

    progName <- getProgName
    args <- getArgs
    let commandLine =
            "// " <> Text.unwords (fmap pack (progName : args)) <> "\n"

    let contents = Text.unlines (fmap commentOutIncludes (Map.elems sourceMap))

    Text.writeFile path (Text.unlines [errorMessage, commandLine, contents])
  where
    commentOutIncludes =
        Text.unlines .
        fmap commentOutInclude .
        Text.lines

    commentOutInclude line
        | "include" `Text.isInfixOf` line = "// " <> line
        | otherwise = line
