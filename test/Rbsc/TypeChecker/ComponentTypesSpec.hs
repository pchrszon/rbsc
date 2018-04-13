{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.ComponentTypesSpec (spec) where


import Control.Lens

import Test.Hspec


import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Result

import Rbsc.TypeChecker.ModelInfo


spec :: Spec
spec = describe "validateComponentTypes" $ do
    it "detects undefined types" $
        toEither (getModelInfo 10
            [model|
                role type R(Undefined);
            |])
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._UndefinedType)

    it "detects non-role types in compartments" $
        toEither (getModelInfo 10
            [model|
                natural type N;
                compartment type C(N);
            |])
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._NonRoleInCompartment)
