{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Rbsc.Parser
    ( parse
    , SourceMap
    ) where


import Control.Lens
import Control.Monad.Except

import           Data.Either        (partitionEithers)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.String
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text

import System.Directory
import System.FilePath

import Text.Megaparsec       hiding (parse)
import Text.Megaparsec.Error (parseErrorTextPretty)


import Rbsc.Parser.ComponentType
import Rbsc.Parser.Constant
import Rbsc.Parser.Coordinator
import Rbsc.Parser.Definition
import Rbsc.Parser.Enumeration
import Rbsc.Parser.Expr
import Rbsc.Parser.Function
import Rbsc.Parser.Global
import Rbsc.Parser.Impl
import Rbsc.Parser.Label
import Rbsc.Parser.Lexer
import Rbsc.Parser.Observe
import Rbsc.Parser.RewardStruct
import Rbsc.Parser.System

import           Rbsc.Report.Error  (Error (..), LocError (..))
import qualified Rbsc.Report.Error  as Error
import qualified Rbsc.Report.Region as Region
import           Rbsc.Report.Result (Result, fromEither)

import Rbsc.Syntax.Untyped


-- | Parse a source file.
parse :: MonadIO m => FilePath -> Text -> [Text] -> m (Result Model, SourceMap)
parse path content constArgContents =
    fmap getResult . withConstants $ \args -> do
        (result, sourceMap) <- run modelFile path content args

        return $ case result of
            Left  err         -> Left [fromParseErrorBundle sourceMap err]
            Right errorOrDefs -> do
                let (errors, defs) = partitionEithers errorOrDefs
                if null errors
                    then return (toModel defs, sourceMap)
                    else throwError errors
  where
    getResult :: Either [Error] (Model, SourceMap) -> (Result Model, SourceMap)
    getResult r =
        let sourceMap = getSourceMap r
        in  (fromEither (over _Right fst r), sourceMap)

    getSourceMap = \case
        Right (_, sourceMap) -> sourceMap
        Left  _              -> Map.empty

    withConstants m = do
        results <- traverse parseCliConst constArgContents
        let (errors, args) = partitionEithers results
        if null errors
            then m (Map.fromList args)
            else return (Left errors)


modelFile :: MonadIO m => ParserT m [ErrorOrDef]
modelFile =
    concat <$> between sc eof (many (include <|> definitions))


definitions :: Parser [ErrorOrDef]
definitions = fmap leftToList . withRecoveryOn (semi <|> symbol "}") $
    componentTypeDef <|> (fmap (: []) . choice $
    [ typeSetDef
    , constantDef
    , enumerationDef
    , functionDef
    , systemDef
    , globalDef
    , labelDef
    , implementationDef
    , moduleDef
    , coordinatorDef
    , rewardStructDef
    , observeDef
    ])
  where
    leftToList = \case
        Left e   -> [Left e]
        Right xs -> fmap Right xs


include :: MonadIO m => ParserT m [ErrorOrDef]
include = do
    void (reserved "include")
    includePath <- unLoc <$> stringLiteral

    -- includePath is relative to file containing the include keyword,
    -- thus we need to make the path relative to our current working
    -- directory
    parentPath <- sourceName <$> getSourcePos
    let parentDir = dropFileName parentPath
        path = parentDir </> includePath

    exists <- liftIO (doesFileExist path)
    if not exists
        then fail ("file " ++ path ++ " does not exist")
        else do
            seen <- Map.member path <$> use sources
            if seen
                then return []
                else parseIncludeFile path


parseIncludeFile :: MonadIO m => FilePath -> ParserT m [ErrorOrDef]
parseIncludeFile path = do
    content <- liftIO (Text.readFile path)
    sources.at path ?= content

    -- save current parser state
    source <- use currentSource
    s      <- getParserState

    -- switch to include file
    setParserState (parserInitState content path)
    currentSource .= content

    -- parse include file
    result <- modelFile

    -- switch back to current file
    currentSource .= source
    setParserState s

    return result
  where
    parserInitState s name = State
        { stateInput    = s
        , stateOffset   = 0
        , statePosState = PosState
            { pstateInput      = s
            , pstateOffset     = 0
            , pstateSourcePos  = initialPos name
            , pstateTabWidth   = defaultTabWidth
            , pstateLinePrefix = ""
            }
        }


parseCliConst :: Monad m => Text -> m (Either Error (Name, LExpr))
parseCliConst = run' cliConstantDef "<command line>"
  where
    cliConstantDef = (,) <$> (unLoc <$> identifier) <*> (equals *> expr)

    run' :: Monad m => ParserT m a -> FilePath -> Text -> m (Either Error a)
    run' p path content = do
        (result, sourceMap) <- run p path content Map.empty
        return (over _Left (fromParseErrorBundle sourceMap) result)


fromParseErrorBundle
    :: (Stream s, ShowErrorComponent e)
    => SourceMap
    -> ParseErrorBundle s e
    -> Error
fromParseErrorBundle sourceMap ParseErrorBundle{..} =
    LocError (MkLocError rgn (Error.ParseError msg))
  where
    rgn = Region.Region path content start end
    msg = fromString (parseErrorTextPretty err)

    path    = sourceName pos
    content = fromMaybe Text.empty (Map.lookup path sourceMap)

    start = fromSourcePos pos
    end   = start { Region.column = Region.column start + 1 }
    pos   = fst (reachOffsetNoLine (errorOffset err) bundlePosState)

    err = NonEmpty.head bundleErrors
