module Rbsc.Compiler where


import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc.Render.Terminal


import qualified Rbsc.Data.ComponentType as CompTy

import Rbsc.Parser

import           Rbsc.Report
import qualified Rbsc.Report.Error.Syntax as Syntax

import Rbsc.TypeChecker.Identifiers
import Rbsc.TypeChecker.Dependencies


compile :: FilePath -> IO (Maybe [Dependency])
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content
    case parseResult of
        Left errors -> printErrors Syntax.toReport errors
        Right model -> case CompTy.fromModel model of
            Left errors -> printErrors Syntax.toReport errors
            Right types -> case identifierDefs model of
                Left errors -> printErrors Syntax.toReport errors
                Right idents -> case sortDefinitions idents of
                    Left err -> printErrors Syntax.toReport [err]
                    Right deps -> return (Just deps)


printErrors :: (a -> Report) -> [a] -> IO (Maybe b)
printErrors toReport errors = do
    _ <- traverse (putDoc . render . toReport) errors
    return Nothing
