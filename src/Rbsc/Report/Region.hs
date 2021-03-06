{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}


-- | Positions and regions in source files.
module Rbsc.Report.Region
    ( Position(..)
    , Region(..)
    , LineRegion(..)
    , Loc(..)
    , withLocOf
    , split
    ) where


import Data.Ord
import Data.Text                 (Text)
import Data.Text.Prettyprint.Doc


-- | A position in a source file.
data Position = Position
    { line   :: !Int
    , column :: !Int
    } deriving (Eq, Ord)

instance Show Position where
    show (Position l c) = show l ++ "," ++ show c


-- | A region within a source file.
data Region = Region
    { path   :: FilePath  -- ^ path of the source file
    , source :: !Text     -- ^ content of the source file this region is referring to
    , start  :: !Position -- ^ start of the region (inclusive)
    , end    :: !Position -- ^ end of the region (exclusive)
    }

instance Eq Region where
    Region lPath _ lStart lEnd == Region rPath _ rStart rEnd =
        lPath == rPath && lStart == rStart && lEnd == rEnd

instance Ord Region where
    compare = comparing path <> comparing start <> comparing end

instance Show Region where
    show r = show (path r) ++ ":" ++ show (start r) ++ "-" ++ show (end r)

instance Semigroup Region where
    Region path1 source1 start1 end1 <> Region _ _ start2 end2 =
        Region path1 source1 (minimum positions) (maximum positions)
      where
        positions = [start1, end1, start2, end2]


-- | A region within a source file spanning contained within a single line.
data LineRegion = LineRegion
    { lrLine  :: !Int      -- ^ source line
    , lrStart :: !Int      -- ^ start column (inclusive)
    , lrEnd   :: Maybe Int -- ^ end column (@Nothing@ represents the end of the line)
    } deriving (Eq, Show)


-- | A value annotated with a 'Region'.
data Loc a = Loc
    { unLoc  :: a
    , getLoc :: !Region
    } deriving (Functor, Foldable, Traversable)

instance Eq a => Eq (Loc a) where
    Loc x _ == Loc y _ = x == y

instance Ord a => Ord (Loc a) where
    compare = comparing unLoc

instance Show a => Show (Loc a) where
    showsPrec d = showsPrec d . unLoc

instance Pretty a => Pretty (Loc a) where
    pretty = pretty . unLoc


-- | @withLocOf x y@ annotates x with the 'Region' of @y@.
withLocOf :: a -> Loc b -> Loc a
withLocOf x (Loc _ rgn) = Loc x rgn


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
