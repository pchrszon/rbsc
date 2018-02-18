module Rbsc.UtilSpec (spec) where


import Data.List.NonEmpty (NonEmpty(..))

import Test.Hspec


import Rbsc.Util


spec :: Spec
spec = describe "topoSort" $ do
    it "finds a topological ordering" $
        topoSort nodes (outgoing edges) `shouldBe` Right [4, 3, 2, 1]
    it "finds cycles" $
        topoSort nodes (outgoing ((3, [1]) : edges)) `shouldBe`
        Left (1 :| [2, 3, 1])


nodes :: [Int]
nodes = [1 .. 4]


outgoing :: [(Int, [Int])] -> Int -> [Int]
outgoing es n = case lookup n es of
    Just ns' -> ns'
    Nothing  -> []


edges :: [(Int, [Int])]
edges =
    [ (1, [2])
    , (2, [3, 4])
    , (3, [4])
    ]
