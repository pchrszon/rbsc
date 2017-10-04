-- | Positions and spans in source files.
module Rbsc.SourceSpan
    ( SourcePos(..)
    , SourceSpan(..)
    ) where


-- | A position in a source file.
data SourcePos = SourcePos
    { sourceLine   :: !Int
    , sourceColumn :: !Int
    } deriving (Show)


-- | A span within a source file.
data SourceSpan = SourceSpan
    { spanFrom :: !SourcePos -- ^ start of the span (inclusive)
    , spanTo   :: !SourcePos -- ^ end of the span (exclusive)
    } deriving (Show)
