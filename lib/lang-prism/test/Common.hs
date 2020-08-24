module Common
    ( module Test.Hspec
    , shouldBe'
    ) where


import Data.Text.Prettyprint.Doc
import Test.Hspec


-- | Tests if pretty printing a value yields the expected result.
shouldBe' :: (HasCallStack, Pretty a) => a -> String -> Expectation
shouldBe' x y = show (pretty x) `shouldBe` y
