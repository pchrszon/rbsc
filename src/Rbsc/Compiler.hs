{-# LANGUAGE GADTs #-}


module Rbsc.Compiler where


import Control.Lens
import Control.Monad

import Data.Foldable
import Data.Text.Prettyprint.Doc.Render.Terminal

import Rbsc.Syntax.Declaration
import qualified Rbsc.ComponentType as CompTy
import qualified Rbsc.SymbolTable as SymbolTable
import Rbsc.Parser
import Rbsc.Report
import qualified Rbsc.Report.Error.Syntax as Syntax
import qualified Rbsc.Report.Error.Type as Type
import qualified Data.Text.IO as Text
import Rbsc.Type
import Rbsc.TypeChecker


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
                    putStrLn ""

                    forOf_ (traverse._DeclSystem) decls $ \cs ->
                        for_ cs $ \c ->
                            case typeCheck types symTable c of
                                Right (AnExpr c' ty) -> case typeEq ty TyBool of
                                    Just Refl -> print c'
                                    Nothing   -> putStrLn "type error"
                                Left err -> putDoc (render (Type.toReport err))


printErrors :: [Syntax.Error] -> IO ()
printErrors errors = void (traverse (putDoc . render . Syntax.toReport) errors)
