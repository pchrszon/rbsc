{-# LANGUAGE GADTs #-}


module Rbsc.Compiler where


import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc.Render.Terminal


import qualified Rbsc.Data.ComponentType as CompTy
import qualified Rbsc.Data.SymbolTable   as SymbolTable

import Rbsc.Parser

import           Rbsc.Report
import qualified Rbsc.Report.Error.Syntax as Syntax
import qualified Rbsc.Report.Error.Type   as Type

import Rbsc.Syntax.Typed

import Rbsc.TypeChecker


compile :: FilePath -> IO (Maybe TModel)
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content
    case parseResult of
        Left errors -> printErrors Syntax.toReport errors
        Right model -> case CompTy.fromModel model of
            Left errors -> printErrors Syntax.toReport errors
            Right types -> case SymbolTable.fromModel types model of
                Left errors -> printErrors Syntax.toReport errors
                Right symTable -> do
                    print types
                    putStrLn ""
                    print symTable
                    putStrLn ""

                    case typeCheck types symTable model of
                        Right model' -> return (Just model')
                        Left err     -> printErrors Type.toReport [err]



printErrors :: (a -> Report) -> [a] -> IO (Maybe b)
printErrors toReport errors = do
    _ <- traverse (putDoc . render . toReport) errors
    return Nothing
