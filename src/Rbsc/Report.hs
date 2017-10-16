{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Report
    ( Report(..)
    , Part(..)
    , render
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
data Report = Report
    { title :: !Text
    , parts :: [Part]
    }


-- | A reference to a code region with an optional description.
data Part = Part
    { region  :: !Region
    , message :: Maybe Text
    }


instance Pretty Report where
    pretty = unAnnotate . render


errorTitleStyle :: AnsiStyle
errorTitleStyle = color Red <> bold


errorUnderlineStyle :: AnsiStyle
errorUnderlineStyle = color Red


errorMessageStyle :: AnsiStyle
errorMessageStyle = color Red <> italicized


lineNumberStyle :: AnsiStyle
lineNumberStyle = colorDull Blue


render :: Report -> Doc AnsiStyle
render (Report title parts) =
    annotate errorTitleStyle (pretty title) <> hardline <>
    mconcat (fmap (renderPart marginWidth) (zip parts prevPaths))
  where
    -- File path of the previos report part. The first part does not have
    -- a predecessor.
    prevPaths = Nothing : fmap (Just . Region.path . region) parts

    marginWidth
        | null parts = 0
        | otherwise  = length (show maxLineNum)

    maxLineNum = maximum (fmap (Region.line . Region.end . region) parts)


renderPart :: Int -> (Part, Maybe FilePath) -> Doc AnsiStyle
renderPart marginWidth (Part region message, path) =
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
        (renderLineRegion marginWidth)
        (zip relevantLines (Region.split region))

    partMessage = maybe emptyDoc
        ((space <>) . annotate errorMessageStyle . pretty) message

    relevantLines =
        take numLines (drop (firstLine - 1) (Text.lines (Region.source region)))

    firstLine = Region.line (Region.start region)
    lastLine = Region.line (Region.end region)
    numLines = lastLine - firstLine + 1


renderLineRegion :: Int -> (Text, LineRegion) -> Doc AnsiStyle
renderLineRegion marginWidth (sourceLine, LineRegion lrLine lrStart lrEnd) =
    annotate lineNumberStyle (fill marginWidth (pretty lrLine) <+> pipe) <+>
    pretty sourceLine <> hardline <> spaces marginWidth <+>
    annotate lineNumberStyle pipe <+> underline
  where
    underline = spaces (lrStart - 1) <>
        annotate errorUnderlineStyle (replicateDoc (lrEnd' - lrStart) "^")
    lrEnd' = fromMaybe (Text.length sourceLine + 1) lrEnd


renderStartPosition :: Region -> Doc ann
renderStartPosition (Region path _ (Position startLine startColumn) _) =
    pretty path <> colon <> pretty startLine <> colon <> pretty startColumn


spaces :: Int -> Doc ann
spaces n = replicateDoc n space


replicateDoc :: Int -> Doc ann -> Doc ann
replicateDoc n = mconcat . replicate n
