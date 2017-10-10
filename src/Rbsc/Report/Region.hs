-- | Positions and regions in source files.
module Rbsc.Report.Region
    ( Position(..)
    , Region(..)
    , LineRegion(..)
    , split
    ) where


-- | A position in a source file.
data Position = Position
    { line   :: !Int
    , column :: !Int
    } deriving (Eq, Show)


-- | A region within a source file.
data Region = Region
    { path  :: FilePath  -- ^ path of the source file
    , start :: !Position -- ^ start of the region (inclusive)
    , end   :: !Position -- ^ end of the region (exclusive)
    } deriving (Eq, Show)


-- | A region within a source file spanning contained within a single line.
data LineRegion = LineRegion
    { lrLine  :: !Int      -- ^ source line
    , lrStart :: !Int      -- ^ start column (inclusive)
    , lrEnd   :: Maybe Int -- ^ end column (@Nothing@ represents the end of the line)
    } deriving (Eq, Show)


-- | Split a 'Region' into 'LineRegion's.
split :: Region -> [LineRegion]
split (Region _ (Position startLine startCol) (Position endLine endCol))
    | startLine >= endLine = [LineRegion startLine startCol (Just endCol)]
    | otherwise =
        LineRegion startLine startCol Nothing :
        innerRegions ++ [LineRegion endLine 1 (Just endCol)]
  where
    innerRegions =
        fmap (\l -> LineRegion l 1 Nothing) [startLine + 1 .. endLine - 1]