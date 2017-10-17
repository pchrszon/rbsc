module Rbsc.Compiler where


import Control.Monad

import Data.Text.Prettyprint.Doc.Render.Terminal

import qualified Rbsc.ComponentType as CompTy
import Rbsc.Parser
import Rbsc.Report
import qualified Rbsc.Report.Error.Syntax as Error


compile :: FilePath -> IO ()
compile path = do
    parseResult <- parse path
    case parseResult of
        Left errors -> void (traverse (putDoc . render . Error.toReport) errors)
        Right decls -> case CompTy.fromDeclarations decls of
            Left errors ->
               void (traverse (putDoc . render . Error.toReport) errors)
            Right types -> print types

