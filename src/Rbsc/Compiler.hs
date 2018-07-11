{-# LANGUAGE FlexibleContexts #-}


module Rbsc.Compiler where


import Control.Monad
import Control.Monad.Reader

import Data.Foldable
import Data.Traversable

import           Data.List                                 (intersperse)
import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc                 (pretty)
import           Data.Text.Prettyprint.Doc.Render.Terminal

import qualified Language.Prism as Prism

import System.Process (callCommand)


import Rbsc.Config

import Rbsc.Data.Info
import Rbsc.Data.System

import Rbsc.Instantiation

import Rbsc.Parser

import Rbsc.Report
import Rbsc.Report.Error   as Error
import Rbsc.Report.Result
import Rbsc.Report.Warning as Warning

import           Rbsc.Syntax.Untyped

import Rbsc.Translator

import Rbsc.TypeChecker

import Rbsc.Visualization.System


compile :: FilePath -> IO ()
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content

    let (result, warnings) = toEither' (translateModels 10 parseResult)

    case result of
        Left errors -> do
            printErrors errors
            unless (null errors) (putStrLn "")
            printWarnings warnings
            return ()
        Right results -> do
            printWarnings warnings

            for_ results $ \(sys, model) -> do
                printSystem sys
                print (pretty model)

            -- let systems = fmap fst results
            -- traverse_ printSystem systems
            -- traverse_ (traverse_ (print . pretty)) bodiess'
            -- drawSystems systems


translateModels
    :: RecursionDepth -> Result Model -> Result [(System, Prism.Model)]
translateModels depth parseResult = do
    (model', sysInfos) <- flip runReaderT depth $ do
        model <- lift parseResult
        (model', mi) <- typeCheck model
        sysInfos <- generateInstances model' mi
        return (model', sysInfos)

    for sysInfos $ \(sys, mi) -> do
        model'' <- translateModel model' sys (Info mi depth)
        return (sys, model'')


printSystem :: System -> IO ()
printSystem sys = do
    print (pretty sys)
    putStrLn ""


drawSystems :: [System] -> IO ()
drawSystems syss =
    traverse_
        (\(sys, i) -> drawSystem sys ("model/systems/system_" ++ show i))
        (zip syss [0 :: Integer ..])


drawSystem :: System -> String -> IO ()
drawSystem sys baseName = do
    writeFile (baseName ++ ".dot") (show (visualizeSystem sys))
    callCommand $ "dot -Tpdf " ++ baseName ++ ".dot > " ++ baseName ++ ".pdf"
    callCommand $ "rm " ++ baseName ++ ".dot"


printErrors :: [Error] -> IO ()
printErrors =
    sequence_ . intersperse (putStrLn "") .
    fmap (putDoc . render . Error.toReport)


printWarnings :: [Warning] -> IO ()
printWarnings =
    sequence_ . intersperse (putStrLn "") .
    fmap (putDoc . render . Warning.toReport)
