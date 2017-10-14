-- | Positions and regions in source files.
module Rbsc.Report.Region
    ( Position(..)
    , Region(..)
    , LineRegion(..)
    , Ann(..)
    , split
    ) where


import Data.Semigroup
import Data.Text (Text)


-- | A position in a source file.
data Position = Position
    { line   :: !Int
    , column :: !Int
    } deriving (Eq, Show)


-- | A region within a source file.
data Region = Region
    { path   :: FilePath  -- ^ path of the source file
    , source :: !Text     -- ^ content of the source file this region is referring to
    , start  :: !Position -- ^ start of the region (inclusive)
    , end    :: !Position -- ^ end of the region (exclusive)
    } deriving (Eq, Show)


instance Semigroup Region where
    Region path1 source1 start1 end1 <> Region _ _ start2 end2 =
        Region
            path1
            source1
            (Position
                 (min (line start1) (line start2))
                 (min (column start1) (column start2)))
            (Position
                 (max (line end1) (line end2))
                 (max (column end1) (column end2)))


-- | A region within a source file spanning contained within a single line.
data LineRegion = LineRegion
    { lrLine  :: !Int      -- ^ source line
    , lrStart :: !Int      -- ^ start column (inclusive)
    , lrEnd   :: Maybe Int -- ^ end column (@Nothing@ represents the end of the line)
    } deriving (Eq, Show)


-- | A value with an annotation.
data Ann t a = Ann
    { unAnn  :: t
    , getAnn :: a
    } deriving (Eq, Show)


-- | Split a 'Region' into 'LineRegion's.
split :: Region -> [LineRegion]
split (Region _ _ (Position startLine startCol) (Position endLine endCol))
    | startLine >= endLine = [LineRegion startLine startCol (Just endCol)]
    | otherwise =
        LineRegion startLine startCol Nothing :
        innerRegions ++ [LineRegion endLine 1 (Just endCol)]
  where
    innerRegions =
        fmap (\l -> LineRegion l 1 Nothing) [startLine + 1 .. endLine - 1]
