{-# LANGUAGE LambdaCase #-}


module GoldenSpec (spec) where


import Control.Lens
import Control.Monad

import           Data.Foldable
import           Data.List        (intercalate, sort)
import           Data.Text        (Text)
import qualified Data.Text.IO     as Text
import           Data.Traversable

import System.Directory
import System.FilePath

import Test.Hspec

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text


import Rbsc.Parser
import Rbsc.Report.Error
import Rbsc.Report.Result
import Rbsc.Translator


goldenDir :: FilePath
goldenDir = "test/golden/"


spec :: Spec
spec = do
    paths <- runIO (findByExtension "rbl" goldenDir)
    traverse_ mkGoldenTest (sort paths)


mkGoldenTest :: FilePath -> Spec
mkGoldenTest path = specify specName $
    translateFile path >>= \case
        Left errors -> expectationFailure $
            intercalate "\n\n" (fmap (show . pretty . toReport) errors)
        Right [model] -> do
            let goldenPath = replaceExtension path "prism"
            goldenExists <- doesFileExist goldenPath
            if goldenExists
                then do
                    golden <- Text.readFile goldenPath

                    when (golden /= model) $ do
                        let actualPath = replaceExtension path "prism.actual"
                        Text.writeFile actualPath model

                    model `shouldBe` golden
                else do
                    Text.writeFile goldenPath model
                    expectationFailure
                        "Golden file did not exist and has been created"
        Right models ->
            expectationFailure $ "got " ++ show (length models) ++ " systems"
  where
    specName = dropExtensions (drop (length goldenDir) path)


translateFile :: FilePath -> IO (Either [Error] [Text])
translateFile path = do
    content <- Text.readFile path

    (parseResult, _) <- parse path content []
    let results = toEither $ do
            model <- parseResult
            toList <$> translateModels 10 model

    return (over (_Right.traverse) (renderPretty . view _3) results)
  where
    renderPretty = renderStrict . layoutPretty defaultLayoutOptions . pretty


findByExtension :: String -> FilePath -> IO [FilePath]
findByExtension ext = go
  where
    go dir = do
        entries <- listDirectory dir
        fmap concat . for entries $ \entry -> do
            let path = dir </> entry
            isDir <- doesDirectoryExist path
            if isDir
                then go path
                else return [path | takeExtension path == ('.' : ext)]
