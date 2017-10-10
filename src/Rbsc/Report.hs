{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Report
    ( render
    , sourceSpan
    , sourcePos
    ) where


import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc

import Rbsc.SourceSpan


render :: Text -> SourceSpan -> Text -> Doc ann
render source srcSpan message =
    sourcePos srcSpan <> hardline <>
    pretty message <> hardline <>
    spaces marginWidth <+> pipe <> hardline <>
    sourceSpan marginWidth source srcSpan <> hardline
  where
    marginWidth = length (show (sourceLine (spanTo srcSpan)))


sourceSpan :: Int -> Text -> SourceSpan -> Doc ann
sourceSpan marginWidth source srcSpan = mconcat (punctuate hardline lineSpans)
  where
    lineSpans =
        fmap (lineSpan marginWidth) (zip sourceLines (splitSourceSpan srcSpan))
    sourceLines = take numLines (drop (firstLine - 1) (Text.lines source))
    firstLine = sourceLine (spanFrom srcSpan)
    lastLine = sourceLine (spanTo srcSpan)
    numLines = lastLine - firstLine + 1


lineSpan :: Int -> (Text, LineSpan) -> Doc ann
lineSpan marginWidth (sourceLineContent, LineSpan l colFrom mColTo) =
    fill marginWidth (pretty l) <+>
    pipe <+>
    pretty sourceLineContent <> hardline <> spaces marginWidth <+>
    pipe <+> underline
  where
    underline = spaces (colFrom - 1) <> replicateDoc (colTo - colFrom) "^"
    colTo = fromMaybe (Text.length sourceLineContent + 1) mColTo


sourcePos :: SourceSpan -> Doc ann
sourcePos (SourceSpan path (SourcePos srcLine srcCol) _) =
    pretty path <> colon <> pretty srcLine <> colon <> pretty srcCol <> colon


spaces :: Int -> Doc ann
spaces n = replicateDoc n space


replicateDoc :: Int -> Doc ann -> Doc ann
replicateDoc n = mconcat . replicate n
