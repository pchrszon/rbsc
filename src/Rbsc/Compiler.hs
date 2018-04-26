module Rbsc.Compiler where


import Control.Monad

import Data.Foldable

import Data.List (intersperse)
import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc                 (pretty)
import           Data.Text.Prettyprint.Doc.Render.Terminal

import System.Process (callCommand)


import Rbsc.Data.System
import Rbsc.Data.ModelInfo

import Rbsc.Instantiation

import Rbsc.Parser

import Rbsc.Report
import Rbsc.Report.Result
import Rbsc.Report.Error as Error
import Rbsc.Report.Warning as Warning

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker

import Rbsc.Visualization.System


compile :: FilePath -> IO ()
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content

    let (result, warnings) = toEither' (generateSystems parseResult)

    case result of
        Left errors -> do
            printErrors errors
            unless (null errors) (putStrLn "")
            printWarnings warnings
        Right results -> do
            printWarnings warnings
            let systems = fmap fst results
            traverse_ printSystem systems
            drawSystems systems


generateSystems :: Result' Model -> Result' [(System, ModelInfo)]
generateSystems parseResult = do
    model          <- parseResult
    (model', info) <- typeCheck 10 model
    generateInstances 10 model' info


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
