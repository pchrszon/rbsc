module Rbsc.SourceSpanSpec (spec) where


import Rbsc.SourceSpan


import Test.Hspec


spec :: Spec
spec = describe "splitSourceSpan" $ do
    it "preserves single line spans" $
        splitSourceSpan (SourceSpan "" (SourcePos 2 3) (SourcePos 2 10))
        `shouldBe`
        [LineSpan 2 3 (Just 10)]
    it "handles two line spans" $
        splitSourceSpan (SourceSpan "" (SourcePos 2 3) (SourcePos 3 10))
        `shouldBe`
        [ LineSpan 2 3 Nothing
        , LineSpan 3 1 (Just 10)
        ]
    it "handles three line spans" $
        splitSourceSpan (SourceSpan "" (SourcePos 2 3) (SourcePos 4 10))
        `shouldBe`
        [ LineSpan 2 3 Nothing
        , LineSpan 3 1 Nothing
        , LineSpan 4 1 (Just 10)
        ]
