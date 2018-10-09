module Rbsc.UtilSpec (spec) where


import           Data.List          (sort, tails)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe
import           Data.Set           (Set, disjoint)
import qualified Data.Set           as Set

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck


import Rbsc.Util


spec :: Spec
spec = do
    describe "topoSort" $ do
        it "finds a topological ordering" $
            topoSort nodes (outgoing edges) `shouldBe` Right [4, 3, 2, 1]
        it "finds cycles" $
            topoSort nodes (outgoing ((3, [1]) : edges)) `shouldBe`
            Left (1 :| [2, 3, 1])

    describe "regions" $ parallel $ do
        prop "returns disjoint regions" $
            allDisjoint . fmap fst . regions . getRegions
        prop "returns each element exactly once" $ \regs ->
            let regs'   = getRegions regs
                xs      = fmap fst (Map.toAscList regs')
                results = regions regs'
                xs'     = sort (concatMap (Set.toList . snd) results)
            in xs == xs'


nodes :: [Int]
nodes = [1 .. 4]


outgoing :: [(Int, [Int])] -> Int -> [Int]
outgoing es n = fromMaybe [] (lookup n es)


edges :: [(Int, [Int])]
edges =
    [ (1, [2])
    , (2, [3, 4])
    , (3, [4])
    ]


newtype CharIntRegions = CharIntRegions
    { getRegions :: Map Char (Set Int)
    } deriving (Eq, Show)

instance Arbitrary CharIntRegions where
    arbitrary = CharIntRegions <$> arbitrary


allDisjoint :: Ord a => [Set a] -> Bool
allDisjoint = all (uncurry disjoint) . pairs
  where
    pairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
