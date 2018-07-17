{-# LANGUAGE RankNTypes #-}


-- | Utility functions for testing.
module Util where


import Control.Lens

import Test.Hspec


import Rbsc.Report.Error


hasError :: Prism' LocErrorDesc a -> Either [Error] b -> Bool
hasError p = has (_Left.traverse._LocError.errorDesc.p)


shouldThrowError
    :: Show a => Either [Error] a -> Prism' LocErrorDesc b -> Expectation
shouldThrowError x p = shouldSatisfy x (hasError p)
