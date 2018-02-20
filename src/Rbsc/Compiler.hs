module Rbsc.Compiler where


import Data.Foldable

import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc.Render.Terminal


import Rbsc.Data.ModelInfo

import Rbsc.Parser

import Rbsc.Report
import Rbsc.Report.Error

import Rbsc.TypeChecker.ModelInfo

import Rbsc.Syntax.Typed


compile :: FilePath -> IO (Maybe (ModelInfo, [TConstant]))
compile path = do
    content <- Text.readFile path
    parseResult <- parse path content
    case parseResult of
        Left errors -> printErrors errors
        Right model -> case getModelInfo 10 model of
            Left errors -> printErrors errors
            Right result -> return (Just result)
        -- Right model -> case getComponentTypes model of
        --     Left errors -> printErrors errors
        --     Right types -> case identifierDefs model of
        --         Left errors -> printErrors errors
        --         Right idents -> case sortDefinitions idents of
        --             Left err   -> printErrors [err]
        --             Right deps -> return (Just deps)


printErrors :: [Error] -> IO (Maybe a)
printErrors errors = do
    traverse_ (putDoc . render . toReport) errors
    return Nothing
