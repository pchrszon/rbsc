{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Report
    ( Report(..)
    , Part(..)
    ) where


import           Data.Maybe                (fromMaybe, isNothing)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc

import           Rbsc.Report.Region (LineRegion (..), Position (..),
                                     Region (..))
import qualified Rbsc.Report.Region as Region


data Report = Report
    { title :: !Text
    , parts :: [Part]
    }


data Part = Part
    { region  :: !Region
    , message :: Maybe Text
    }


instance Pretty Report where
    pretty = render


render :: Report -> Doc ann
render (Report title parts) =
    pretty title <> hardline <>
    mconcat (fmap (renderPart marginWidth) (zip parts prevPaths))
  where
    -- File path of the previos report part. The first part does not have
    -- a predecessor.
    prevPaths = Nothing : fmap (Just . Region.path . region) parts

    marginWidth = length (show maxLineNum)
    maxLineNum = maximum (fmap (Region.line . Region.end . region) parts)


renderPart :: Int -> (Part, Maybe FilePath) -> Doc ann
renderPart marginWidth (Part region message, path) =
    partPosition <> hardline <>
    mconcat (punctuate hardline lineRegions) <> partMessage <> hardline
  where
    partPosition
        | Just (Region.path region) == path = "..."
        | otherwise =
            (if isNothing path then emptyDoc else hardline) <>
            "  --> " <> renderStartPosition region <> hardline <>
            spaces marginWidth <+> pipe

    lineRegions = fmap
        (renderLineRegion marginWidth)
        (zip relevantLines (Region.split region))

    partMessage = maybe emptyDoc ((space <>) . pretty) message

    relevantLines =
        take numLines (drop (firstLine - 1) (Text.lines (Region.source region)))

    firstLine = Region.line (Region.start region)
    lastLine = Region.line (Region.end region)
    numLines = lastLine - firstLine + 1


renderLineRegion :: Int -> (Text, LineRegion) -> Doc ann
renderLineRegion marginWidth (sourceLine, LineRegion lrLine lrStart lrEnd) =
    fill marginWidth (pretty lrLine) <+> pipe <+>
    pretty sourceLine <> hardline <> spaces marginWidth <+>
    pipe <+> underline
  where
    underline = spaces (lrStart - 1) <> replicateDoc (lrEnd' - lrStart) "^"
    lrEnd' = fromMaybe (Text.length sourceLine + 1) lrEnd


renderStartPosition :: Region -> Doc ann
renderStartPosition (Region path _ (Position startLine startColumn) _) =
    pretty path <> colon <> pretty startLine <> colon <> pretty startColumn


spaces :: Int -> Doc ann
spaces n = replicateDoc n space


replicateDoc :: Int -> Doc ann -> Doc ann
replicateDoc n = mconcat . replicate n
