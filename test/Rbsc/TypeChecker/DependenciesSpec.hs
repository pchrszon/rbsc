{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.DependenciesSpec (spec) where


import Control.Arrow ((+++))
import Control.Lens

import Data.Monoid

import Test.Hspec


import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker.Dependencies
import Rbsc.TypeChecker.Identifiers


spec :: Spec
spec = describe "sortDefinitions" $ do
    it "finds a correct ordering of definitions" $
        dependencies
            [model|
                const n = 2;
                function arr(i : int) : array [0..n] of int = f(i);
                function f(i : int) : int = i + n;
                const k = arr(n)[0];
            |]
        `shouldBe`
        Right
            [ "n"
            , "sig_arr"
            , "sig_f"
            , "arr"
            , "f"
            , "k"
            ]

    it "detects cyclic definitions" $
        dependencies
            [model|
                const n = k + 1;
                const k = n + 1;
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._CyclicDefinition)


dependencies :: UModel -> Either [Error] [Name]
dependencies m = do
    idents <- identifierDefs m
    (: []) +++ fmap getName $ sortDefinitions idents


getName :: Dependency -> Name
getName = \case
    DepConstant c          -> unLoc (constName c)
    DepFunction f          -> unLoc (functionName f)
    DepFunctionSignature f -> "sig_" <> unLoc (functionName f)
    DepComponent name _    -> name
