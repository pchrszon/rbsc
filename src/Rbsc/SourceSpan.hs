-- | Positions and spans in source files.
module Rbsc.SourceSpan
    ( Line
    , Column
    , SourcePos(..)
    , SourceSpan(..)
    , LineSpan(..)
    , splitSourceSpan
    ) where


-- | A line number.
type Line = Int


-- | A column number.
type Column = Int


-- | A position in a source file.
data SourcePos = SourcePos
    { sourceLine   :: !Line
    , sourceColumn :: !Column
    } deriving (Eq, Show)


-- | A span within a source file.
data SourceSpan = SourceSpan
    { spanFile :: FilePath   -- ^ path of the source file
    , spanFrom :: !SourcePos -- ^ start of the span (inclusive)
    , spanTo   :: !SourcePos -- ^ end of the span (exclusive)
    } deriving (Eq, Show)


-- | A span within a source file that only spans one line.
data LineSpan = LineSpan
    { lspanLine :: !Line        -- ^ source line
    , lspanFrom :: !Column      -- ^ start column (inclusive)
    , lspanTo   :: Maybe Column -- ^ end column (@Nothing@ represents the end of the line)
    } deriving (Eq, Show)


-- | Split a 'SourceSpan' into 'LineSpan's.
splitSourceSpan :: SourceSpan -> [LineSpan]
splitSourceSpan (SourceSpan _ (SourcePos fromLine fromCol) (SourcePos toLine toCol))
    | fromLine >= toLine = [LineSpan fromLine fromCol (Just toCol)]
    | otherwise =
        LineSpan fromLine fromCol Nothing :
        innerSpans ++ [LineSpan toLine 1 (Just toCol)]
  where
    innerSpans = fmap (\l -> LineSpan l 1 Nothing) [fromLine + 1 .. toLine - 1]
