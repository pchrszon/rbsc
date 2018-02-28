module Rbsc.Compiler where


import Data.Foldable

import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc                 (pretty)
import           Data.Text.Prettyprint.Doc.Render.Terminal

import System.Process (callCommand)


import Rbsc.Data.System

import Rbsc.Instancing

import Rbsc.Parser

import Rbsc.Report
import Rbsc.Report.Error

import Rbsc.TypeChecker

import Rbsc.Visualization.System


compile :: FilePath -> IO (Maybe ())
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content
    case parseResult of
        Left errors -> printErrors errors
        Right model -> case typeCheck 10 model of
            Left errors -> printErrors errors
            Right (model', info) -> case generateInstances 10 model' info of
                Left err -> printErrors [err]
                Right results -> do
                    let systems = fmap fst results
                    traverse_ printSystem systems
                    drawSystems systems
                    return (Just ())


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


printErrors :: [Error] -> IO (Maybe a)
printErrors errors = do
    traverse_ (putDoc . render . toReport) errors
    return Nothing
