module Rbsc.Compiler where


import Data.Foldable

import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc.Render.Terminal


import Rbsc.Data.System

import Rbsc.Instancing

import Rbsc.Parser

import Rbsc.Report
import Rbsc.Report.Error

import Rbsc.TypeChecker


import Rbsc.Syntax.Typed


compile :: FilePath -> IO (Maybe (System, [LSomeExpr]))
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content
    case parseResult of
        Left errors -> printErrors errors
        Right model -> case typeCheck 10 model of
            Left errors -> printErrors errors
            Right (model', info) -> case buildSystem model' info of
                Left err -> printErrors [err]
                Right result -> return (Just result)


printErrors :: [Error] -> IO (Maybe a)
printErrors errors = do
    traverse_ (putDoc . render . toReport) errors
    return Nothing
