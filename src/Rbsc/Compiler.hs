{-# LANGUAGE GADTs #-}


module Rbsc.Compiler where


import Control.Monad

import           Data.Foldable
import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc.Render.Terminal


import qualified Rbsc.Data.ComponentType as CompTy
import qualified Rbsc.Data.SymbolTable   as SymbolTable
import           Rbsc.Data.Type

import Rbsc.Parser

import           Rbsc.Report
import qualified Rbsc.Report.Error.Syntax as Syntax
import qualified Rbsc.Report.Error.Type   as Type

import Rbsc.Syntax.Model

import Rbsc.TypeChecker


compile :: FilePath -> IO ()
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content
    case parseResult of
        Left errors -> printErrors errors
        Right model -> case CompTy.fromModel model of
            Left errors -> printErrors errors
            Right types -> case SymbolTable.fromModel types model of
                Left errors -> printErrors errors
                Right symTable -> do
                    print types
                    putStrLn ""
                    print symTable
                    putStrLn ""

                    for_ (system model) $ \c ->
                        case typeCheck types symTable c of
                            Right (AnExpr c' ty) -> case typeEq ty TyBool of
                                Just Refl -> print c'
                                Nothing   -> putStrLn "type error"
                            Left err -> putDoc (render (Type.toReport err))


printErrors :: [Syntax.Error] -> IO ()
printErrors errors = void (traverse (putDoc . render . Syntax.toReport) errors)
