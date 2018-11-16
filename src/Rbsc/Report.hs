{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Reports referring to a source code region.
module Rbsc.Report
    ( Report
    , errorReport
    , hintReport

    , Part
    , errorPart
    , hintPart

    , render

    , prettyIdent
    , prettyIdents
    ) where


import           Data.Maybe                                (fromMaybe,
                                                            isNothing)
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Rbsc.Report.Region (LineRegion (..), Position (..),
                                     Region (..))
import qualified Rbsc.Report.Region as Region


-- | Represents information about the source code (e.g., an error or a warning).
data Report = Report !MessageType !Text [Part]


-- | A reference to a code region with an optional description.
data Part = Part
    { _type    :: !MessageType
    , _region  :: !Region
    , _message :: Maybe (Doc AnsiStyle)
    }


-- | A 'Part' can either point to an error or give a hint.
data MessageType
    = Error
    | Hint


-- | Create a 'Report' representing an error.
errorReport :: Text -> [Part] -> Report
errorReport = Report Error


-- | Create a 'Report' representing a hint.
hintReport :: Text -> [Part] -> Report
hintReport = Report Hint


-- | Create a 'Part' referencing an error.
errorPart :: Region -> Maybe (Doc AnsiStyle) -> Part
errorPart = Part Error


-- | Create a 'Part' showing a hint.
hintPart :: Region -> Maybe (Doc AnsiStyle) -> Part
hintPart = Part Hint


instance Pretty Report where
    pretty = unAnnotate . render


-- | Render an identifier.
prettyIdent :: Text -> Doc AnsiStyle
prettyIdent = annotate bold . pretty


-- | Render a list of identifiers.
prettyIdents :: [Text] -> [Doc AnsiStyle]
prettyIdents = fmap prettyIdent


errorTitleStyle :: AnsiStyle
errorTitleStyle = color Red <> bold


hintTitleStyle :: AnsiStyle
hintTitleStyle = color Blue <> bold


errorUnderlineStyle :: AnsiStyle
errorUnderlineStyle = color Red <> bold


hintUnderlineStyle :: AnsiStyle
hintUnderlineStyle = color Blue


errorMessageStyle :: AnsiStyle
errorMessageStyle = color Red <> italicized


hintMessageStyle :: AnsiStyle
hintMessageStyle = color Blue <> italicized


lineNumberStyle :: AnsiStyle
lineNumberStyle = colorDull Blue


-- | Render a 'Report'.
render :: Report -> Doc AnsiStyle
render (Report ty title parts) =
    annotate titleStyle (pretty title) <> hardline <>
    mconcat (fmap (renderPart marginWidth) (zip parts prevPaths))
  where
    -- File path of the previos report part. The first part does not have
    -- a predecessor.
    prevPaths = Nothing : fmap (Just . Region.path . _region) parts

    marginWidth
        | null parts = 0
        | otherwise  = length (show maxLineNum)

    maxLineNum = maximum (fmap (Region.line . Region.end . _region) parts)

    titleStyle = case ty of
        Error -> errorTitleStyle
        Hint  -> hintTitleStyle


renderPart :: Int -> (Part, Maybe FilePath) -> Doc AnsiStyle
renderPart marginWidth (Part partType region message, path) =
    partPosition <> hardline <>
    mconcat (punctuate hardline lineRegions) <> partMessage <> hardline
  where
    partPosition
        | Just (Region.path region) == path = "..."
        | otherwise =
            (if isNothing path then emptyDoc else hardline) <>
            "  --> " <> renderStartPosition region <> hardline <>
            spaces marginWidth <+> annotate lineNumberStyle pipe

    lineRegions = fmap
        (renderLineRegion marginWidth partType)
        (zip relevantLines (Region.split region))

    partMessage = maybe emptyDoc
        ((space <>) . annotate messageStyle . hang 0) message

    messageStyle =
        case partType of
            Error -> errorMessageStyle
            Hint  -> hintMessageStyle

    relevantLines =
        take numLines (drop (firstLine - 1) (Text.lines (Region.source region)))

    firstLine = Region.line (Region.start region)
    lastLine = Region.line (Region.end region)
    numLines = lastLine - firstLine + 1


renderLineRegion :: Int -> MessageType -> (Text, LineRegion) -> Doc AnsiStyle
renderLineRegion marginWidth partType (sourceLine, LineRegion lrLine lrStart lrEnd) =
    annotate lineNumberStyle (fill marginWidth (pretty lrLine) <+> pipe) <+>
    prettySourceLine <> hardline <> spaces marginWidth <+>
    annotate lineNumberStyle pipe <+> underline
  where
    prettySourceLine =
        let (prefix, sourceLine') = Text.splitAt (lrStart - 1) sourceLine
            (lineRegion, suffix) = Text.splitAt (lrEnd' - lrStart) sourceLine'
        in pretty prefix <> annotate ulStyle (pretty lineRegion) <> pretty suffix

    underline = spaces (lrStart - 1) <>
        annotate ulStyle (replicateDoc (lrEnd' - lrStart) ulChar)

    ulStyle =
        case partType of
            Error -> errorUnderlineStyle
            Hint  -> hintUnderlineStyle

    ulChar =
        case partType of
            Error -> "^"
            Hint  -> "-"

    lrEnd' = fromMaybe (Text.length sourceLine + 1) lrEnd


renderStartPosition :: Region -> Doc ann
renderStartPosition (Region path _ (Position startLine startColumn) _) =
    pretty path <> colon <> pretty startLine <> colon <> pretty startColumn


spaces :: Int -> Doc ann
spaces n = replicateDoc n space


replicateDoc :: Int -> Doc ann -> Doc ann
replicateDoc n = mconcat . replicate n
