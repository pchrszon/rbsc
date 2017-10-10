{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Report
    ( render
    ) where


import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc

import Rbsc.Report.Region (Region(..), LineRegion(..), Position(..))
import qualified Rbsc.Report.Region as Region


render :: Region -> Text -> Doc ann
render region message =
    renderStartPosition region <> hardline <>
    pretty message <> hardline <>
    spaces marginWidth <+> pipe <> hardline <>
    renderRegion marginWidth region <> hardline
  where
    marginWidth = length (show (Region.line (Region.end region)))


renderRegion :: Int -> Region -> Doc ann
renderRegion marginWidth region =
    mconcat (punctuate hardline lineRegions)
  where
    lineRegions = fmap
        (renderLineRegion marginWidth)
        (zip relevantLines (Region.split region))

    relevantLines =
        take numLines (drop (firstLine - 1) (Text.lines (Region.source region)))

    firstLine = Region.line (Region.start region)
    lastLine = Region.line (Region.end region)
    numLines = lastLine - firstLine + 1


renderLineRegion :: Int -> (Text, LineRegion) -> Doc ann
renderLineRegion marginWidth (sourceLine, LineRegion lrLine lrStart lrEnd) =
    fill marginWidth (pretty lrLine) <+>
    pipe <+>
    pretty sourceLine <> hardline <> spaces marginWidth <+>
    pipe <+> underline
  where
    underline = spaces (lrStart - 1) <> replicateDoc (lrEnd' - lrStart) "^"
    lrEnd' = fromMaybe (Text.length sourceLine + 1) lrEnd


renderStartPosition :: Region -> Doc ann
renderStartPosition (Region path _ (Position startLine startColumn) _) =
    pretty path <> colon <> pretty startLine <> colon <> pretty startColumn <>
    colon


spaces :: Int -> Doc ann
spaces n = replicateDoc n space


replicateDoc :: Int -> Doc ann -> Doc ann
replicateDoc n = mconcat . replicate n
