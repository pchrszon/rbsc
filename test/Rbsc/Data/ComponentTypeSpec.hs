{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.Data.ComponentTypeSpec (spec) where


import Control.Lens

import Test.Hspec

import Rbsc.Data.ComponentType
import Rbsc.Parser.TH
import Rbsc.Report.Error


spec :: Spec
spec = describe "fromModel" $ do
    it "extracts all types" $
        fromModel
            [model|
                natural type N;
                role type R(N);
                compartment type C(R);
            |]
        `shouldBe`
        Right
            [ ("N", NaturalType)
            , ("R", RoleType ["N"])
            , ("C", CompartmentType ["R"])
            ]

    it "detects undefined types" $
        fromModel [model| role type R(Undefined); |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._UndefinedType)

    it "detects duplicate type definitions" $
        fromModel
            [model|
                natural type N;
                natural type N;
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._DuplicateType)

    it "detects non-role types in compartments" $
        fromModel
            [model|
                natural type N;
                compartment type C(N);
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._NonRoleInCompartment)
