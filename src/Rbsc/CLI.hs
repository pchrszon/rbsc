{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.CLI
    ( rbscMain
    ) where


import Control.Exception.Lens
import Control.Lens
import Control.Monad.Reader

import           Data.Foldable
import           Data.List                                 (intersperse)
import           Data.List.NonEmpty                        (NonEmpty)
import qualified Data.List.NonEmpty                        as NonEmpty
import           Data.Maybe
import           Data.Text                                 (Text, pack)
import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as T
import qualified Data.Text.Prettyprint.Doc.Render.Text     as U

import qualified Language.Prism         as Prism
import qualified Language.Prism.Convert as Prism

import System.FilePath
import System.IO


import Rbsc.CLI.ExceptionHandlers
import Rbsc.CLI.Options
import Rbsc.CLI.Parser

import Rbsc.Config

import Rbsc.Data.ModelInfo
import Rbsc.Data.System

import Rbsc.Parser

import           Rbsc.Report
import           Rbsc.Report.Error   (Error)
import qualified Rbsc.Report.Error   as Error
import           Rbsc.Report.Result
import           Rbsc.Report.Warning (Warning)
import           Rbsc.Report.Warning as Warning

import Rbsc.Syntax.Typed.Expr (prettyConstants)
import Rbsc.Syntax.Untyped    (Model)

import Rbsc.Translator
import Rbsc.Translator.Convert

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

    consts <- asks optConstants
    (parseResult, sourceMap) <- parse path content consts

    handling _ErrorCall (lift . errorHandler sourceMap) $ do
        depth        <- asks optRecursionDepth
        multiActions <- asks optMultiActions

        let (result, warnings) = toEither' (translate depth parseResult)

        case result of
            Right results -> do
                printWarnings warnings

                results' <- if multiActions
                    then return results
                    else convertAll results

                handleResults results'
            Left errors -> do
                printWarnings warnings
                unless (null warnings) (liftIO (putStrLn ""))
                printErrors errors


handleResults :: NonEmpty (System, ModelInfo, Prism.Model) -> App ()
handleResults results = do
    path         <- getOutputPath
    mSysPath     <- asks optExportSystems
    mDiagramPath <- asks optExportDiagrams

    hasPrintConsts <- asks optPrintConstants
    when hasPrintConsts (printConsts (view _2 (NonEmpty.head results)))

    putStrLnVerbose $ "Generated " <> if numResults == 1
        then "1 system\n"
        else pack (show numResults) <> " systems\n"

    w <- asks optPageWidth
    let layoutOpts = LayoutOptions $
            if w <= 0 then Unbounded else AvailablePerLine w 1.0

    for_ iresults $ \(i, (sys, _, model')) -> do
        putStrLnVerbose (renderPretty sys)
        when (fromInteger i < numResults - 1) (putStrLnVerbose "")

        writeDoc layoutOpts (pretty model') i path
        whenIsJust mSysPath (writeDoc layoutOpts (pretty sys) i)
        whenIsJust mDiagramPath (writeDoc layoutOpts (visualizeSystem sys) i)
  where
    iresults = zip [0 :: Integer ..] (toList results)

    getOutputPath = fromMaybe "out.prism" <$> asks optOutput

    writeDoc opts doc i path =
        liftIO . withFile (addFileNameIndex path i) WriteMode $ \h ->
            U.renderIO h (layoutPretty opts doc)

    printConsts = liftIO . Text.putStrLn . renderDefault . vsep .
        prettyConstants . view constants

    addFileNameIndex path i
        | length results > 1 =
            let (name, ext) = splitExtension path
            in name ++ "_" ++ show i ++ ext
        | otherwise = path

    numResults = length results

    renderPretty  = renderDefault . pretty
    renderDefault = U.renderStrict . layoutPretty defaultLayoutOptions


readModel :: App (FilePath, Text)
readModel = asks optInput >>= \case
    "-"  -> (,) "<stdin>" <$> liftIO Text.getContents
    path -> (,) path <$> liftIO (Text.readFile path)


translate
    :: RecursionDepth
    -> Result Model
    -> Result (NonEmpty (System, ModelInfo, Prism.Model))
translate depth parseResult = do
    model <- parseResult
    translateModels depth model


convertAll
    :: NonEmpty (System, ModelInfo, Prism.Model)
    -> App (NonEmpty (System, ModelInfo, Prism.Model))
convertAll = (traverse._3) convert


convert :: Prism.Model -> App Prism.Model
convert model = do
    let ci = prepareConversion model
    putStrLnVerbose "Conversion to single actions"
    putStrLnVerbose $
        "size of alphabet: " <> pack (show (Prism.numberOfActions ci))
    putStrLnVerbose $
        "number of composed multi-actions: " <>
        pack (show (Prism.numberOfMultiActions ci))
    putStrLnVerbose ""
    return (convertToSingleActions ci model)


printErrors :: [Error] -> App ()
printErrors = printReports Error.toReport stderr


printWarnings :: [Warning] -> App ()
printWarnings ws = do
    showWarnings <- asks optShowWarnings
    showNoSyncWarnings <- asks optWarnNoSync
    let ws' = if showNoSyncWarnings
                  then ws
                  else filter (hasn't _UnsynchronizedAction) ws
    when showWarnings (printReports Warning.toReport stderr ws')


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
