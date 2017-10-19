{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.ComponentTypeSpec (spec) where


import Control.Lens

import Test.Hspec

import Rbsc.ComponentType
import qualified Rbsc.Report.Error.Syntax as Syntax
import Rbsc.Parser.TH


spec :: Spec
spec = describe "fromDeclarations" $ do
    it "extracts all types" $
        fromDeclarations
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
        fromDeclarations [model| role type R(Undefined); |]
        `shouldSatisfy`
        has (_Left.traverse.Syntax._UndefinedType)

    it "detects duplicate type definitions" $
        fromDeclarations
            [model|
                natural type N;
                natural type N;
            |]
        `shouldSatisfy`
        has (_Left.traverse.Syntax._DuplicateType)

    it "detects non-role types in compartments" $
        fromDeclarations
            [model|
                natural type N;
                compartment type C(N);
            |]
        `shouldSatisfy`
        has (_Left.traverse.Syntax._NonRoleInCompartment)
