{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.CLI
    ( rbscMain
    ) where


import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Control.Monad.Reader

import           Data.Foldable
import           Data.List                                 (intersperse)
import           Data.Maybe
import           Data.Text                                 (Text, pack)
import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as T
import qualified Data.Text.Prettyprint.Doc.Render.Text     as U

import qualified Language.Prism as Prism

import System.Exit
import System.FilePath
import System.IO
import System.IO.Error.Lens


import Rbsc.CLI.Options
import Rbsc.CLI.Parser

import Rbsc.Config

import Rbsc.Data.System

import Rbsc.Parser

import           Rbsc.Report
import           Rbsc.Report.Error   (Error)
import qualified Rbsc.Report.Error   as Error
import           Rbsc.Report.Result
import           Rbsc.Report.Warning (Warning)
import           Rbsc.Report.Warning as Warning

import Rbsc.Syntax.Untyped (Model)

import Rbsc.Translator

import Rbsc.Util (whenIsJust)

import Rbsc.Visualization.System


rbscMain :: IO ()
rbscMain =
    handling _IOException ioExceptionHandler (parseOptions >>= runApp rbsc)


type App a = ReaderT Options IO a


runApp :: App a -> Options -> IO a
runApp = runReaderT


rbsc :: App ()
rbsc = do
    (path, content) <- readModel
    parseResult <- parse path content

    depth <- asks optRecursionDepth
    let (result, warnings) = toEither' (translate depth parseResult)

    case result of
        Right results -> do
            printWarnings warnings
            handleResults results
        Left errors -> do
            printWarnings warnings
            unless (null warnings) (liftIO (putStrLn ""))
            printErrors errors


handleResults :: [(System, Prism.Model)] -> App ()
handleResults results = do
    path         <- getOutputPath
    mSysPath     <- asks optExportSystems
    mDiagramPath <- asks optExportDiagrams

    putStrLnVerbose $ "Generated " <> if numResults == 1
        then "1 system\n"
        else pack (show numResults) <> " systems\n"

    for_ iresults $ \(i, (sys, model')) -> do
        putStrLnVerbose (renderPretty sys)
        when (fromInteger i < numResults - 1) (putStrLnVerbose "")

        writeDoc (pretty model') i path
        whenIsJust mSysPath (writeDoc (pretty sys) i)
        whenIsJust mDiagramPath (writeDoc (visualizeSystem sys) i)
  where
    iresults = zip [0 :: Integer ..] results

    getOutputPath = fromMaybe "out.prism" <$> asks optOutput

    writeDoc doc i path =
        liftIO . withFile (addFileNameIndex path i) WriteMode $ \h ->
            U.hPutDoc h doc

    addFileNameIndex path i
        | length results > 1 =
            let (name, ext) = splitExtension path
            in name ++ "_" ++ show i ++ ext
        | otherwise = path

    numResults = length results

    renderPretty = U.renderStrict . layoutPretty defaultLayoutOptions . pretty


readModel :: App (FilePath, Text)
readModel = asks optInput >>= \case
    Just path -> (,) path <$> liftIO (Text.readFile path)
    Nothing   -> (,) "<stdin>" <$> liftIO Text.getContents


translate :: RecursionDepth -> Result Model -> Result [(System, Prism.Model)]
translate depth parseResult = do
    model <- parseResult
    translateModels depth model


printErrors :: [Error] -> App ()
printErrors = printReports Error.toReport stderr


printWarnings :: [Warning] -> App ()
printWarnings ws = do
    showWarnings <- asks optShowWarnings
    when showWarnings (printReports Warning.toReport stderr ws)


printReports :: (a -> Report) -> Handle -> [a] -> App ()
printReports f h xs = do
    colored <- asks optShowColor
    let p = if colored then putColor else putNoColor

    liftIO (sequence_ (intersperse (putStrLn "") (fmap (p . toDocStream) xs)))
  where
    toDocStream = layoutPretty defaultLayoutOptions . render . f

    putColor   = T.renderIO h
    putNoColor = U.renderIO h


putStrLnVerbose :: Text -> App ()
putStrLnVerbose s = asks optVerbose >>= \case
    Verbose    -> liftIO (Text.putStrLn s)
    NonVerbose -> return ()


ioExceptionHandler :: IOException -> IO a
ioExceptionHandler e = do
    let file = fromMaybe "<unknown source>" (view fileName e)
    hPutStrLn stderr (view description e ++ ": " ++ file)
    exitWith (ExitFailure 2)
