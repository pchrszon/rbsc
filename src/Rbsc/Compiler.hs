module Rbsc.Compiler where


import Control.Monad

import Data.Text.Prettyprint.Doc.Render.Terminal

import qualified Rbsc.ComponentType as CompTy
import qualified Rbsc.SymbolTable as SymbolTable
import Rbsc.Parser
import Rbsc.Report
import qualified Rbsc.Report.Error.Syntax as Error
import qualified Data.Text.IO as Text


compile :: FilePath -> IO ()
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content
    case parseResult of
        Left errors -> printErrors errors
        Right decls -> case CompTy.fromDeclarations decls of
            Left errors -> printErrors errors
            Right types -> case SymbolTable.fromDeclarations types decls of
                Left errors -> printErrors errors
                Right symTable -> do
                    print types
                    putStrLn ""
                    print symTable


printErrors :: [Error.Error] -> IO ()
printErrors errors = void (traverse (putDoc . render . Error.toReport) errors)
