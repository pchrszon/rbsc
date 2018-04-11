{-# LANGUAGE OverloadedStrings #-}


module Rbsc.ReportSpec (spec) where


import Data.Monoid
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Pretty(..))

import Test.Hspec

import Rbsc.Report
import Rbsc.Report.Region


spec :: Spec
spec = describe "pretty" $ do
    it "underlines the region" $ shouldRender
        (errorReport "title" [errorPart (mkRegion (2, 1) (2, 7)) Nothing]) $
        "title\n" <>
        "  --> <test>:2:1\n" <>
        "  |\n" <>
        "2 | second line\n" <>
        "  | ^^^^^^\n"

    it "prints a message" $ shouldRender
        (errorReport "title" [errorPart (mkRegion (2, 1) (2, 7)) (Just "message")]) $
        "title\n" <>
        "  --> <test>:2:1\n" <>
        "  |\n" <>
        "2 | second line\n" <>
        "  | ^^^^^^ message\n"

    it "handles multi-line regions" $ shouldRender
        (errorReport "title" [errorPart (mkRegion (1, 7) (3, 6)) Nothing]) $
        "title\n" <>
        "  --> <test>:1:7\n" <>
        "  |\n" <>
        "1 | first line\n" <>
        "  |       ^^^^\n" <>
        "2 | second line\n" <>
        "  | ^^^^^^^^^^^\n" <>
        "3 | third line\n" <>
        "  | ^^^^^\n"

    it "prints multiple parts" $ shouldRender
        (errorReport "title"
            [ errorPart (mkRegion (1, 1) (1, 6)) (Just "first")
            , hintPart (mkRegion (3, 1) (3, 6)) (Just "second")
            ]) $
        "title\n" <>
        "  --> <test>:1:1\n" <>
        "  |\n" <>
        "1 | first line\n" <>
        "  | ^^^^^ first\n" <>
        "...\n" <>
        "3 | third line\n" <>
        "  | ----- second\n"

    it "prints multiple parts from different sources" $ shouldRender
        (errorReport "title"
            [ errorPart (mkRegion (2, 1) (2, 7)) (Just "first")
            , hintPart ((mkRegion (2, 7) (2, 13))
                { path = "<other>", source = otherSource }) (Just "second")
            ]) $
        "title\n" <>
        "  --> <test>:2:1\n" <>
        "  |\n" <>
        "2 | second line\n" <>
        "  | ^^^^^^ first\n" <>
        "\n" <>
        "  --> <other>:2:7\n" <>
        "  |\n" <>
        "2 | other second line\n" <>
        "  |       ------ second\n"


testSource :: Text
testSource =
    "first line\n" <>
    "second line\n" <>
    "third line\n"

otherSource :: Text
otherSource =
    "other first line\n" <>
    "other second line\n"


mkRegion :: (Int, Int) -> (Int, Int) -> Region
mkRegion (startLine, startColumn) (endLine, endColumn) =
    Region
        "<test>"
        testSource
        (Position startLine startColumn)
        (Position endLine endColumn)


shouldRender :: Pretty a => a -> String -> Expectation
shouldRender = shouldBe . show . pretty
