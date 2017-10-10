module Rbsc.Report.RegionSpec (spec) where


import Rbsc.Report.Region


import Test.Hspec


spec :: Spec
spec = describe "split" $ do
    it "preserves single line regions" $
        split (Region "" (Position 2 3) (Position 2 10))
        `shouldBe`
        [LineRegion 2 3 (Just 10)]
    it "handles two line regions" $
        split (Region "" (Position 2 3) (Position 3 10))
        `shouldBe`
        [ LineRegion 2 3 Nothing
        , LineRegion 3 1 (Just 10)
        ]
    it "handles three line regions" $
        split (Region "" (Position 2 3) (Position 4 10))
        `shouldBe`
        [ LineRegion 2 3 Nothing
        , LineRegion 3 1 Nothing
        , LineRegion 4 1 (Just 10)
        ]
