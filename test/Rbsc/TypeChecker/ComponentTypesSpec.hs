{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.ComponentTypesSpec (spec) where


import Control.Lens

import Test.Hspec


import Rbsc.Data.ComponentType

import Rbsc.Parser.TH

import Rbsc.Report.Error

import Rbsc.TypeChecker.ComponentTypes


spec :: Spec
spec = describe "getComponentTypes" $ do
    it "extracts all types" $
        getComponentTypes
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
        getComponentTypes [model| role type R(Undefined); |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._UndefinedType)

    it "detects duplicate type definitions" $
        getComponentTypes
            [model|
                natural type N;
                natural type N;
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._DuplicateType)

    it "detects non-role types in compartments" $
        getComponentTypes
            [model|
                natural type N;
                compartment type C(N);
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._NonRoleInCompartment)
