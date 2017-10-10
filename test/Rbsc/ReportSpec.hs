{-# LANGUAGE OverloadedStrings #-}


module Rbsc.ReportSpec (spec) where


import Data.Monoid
import Data.Text (Text)

import Test.Hspec

import Rbsc.Report
import Rbsc.SourceSpan


spec :: Spec
spec = describe "render" $ do
    it "underlines the span" $
        let s = SourceSpan "test" (SourcePos 2 1) (SourcePos 2 7)
            msg = "test report message"
        in shouldBe (show (render source s msg)) $
            "test:2:1:\n" <>
            "test report message\n" <>
            "  |\n" <>
            "2 | second line\n" <>
            "  | ^^^^^^\n"
    it "handles multi-line spans" $
        let s = SourceSpan "test" (SourcePos 1 7) (SourcePos 3 6)
            msg = "test report message"
        in shouldBe (show (render source s msg)) $
            "test:1:7:\n" <>
            "test report message\n" <>
            "  |\n" <>
            "1 | first line\n" <>
            "  |       ^^^^\n" <>
            "2 | second line\n" <>
            "  | ^^^^^^^^^^^\n" <>
            "3 | third line\n" <>
            "  | ^^^^^\n"



source :: Text
source =
    "first line\n" <>
    "second line\n" <>
    "third line\n"
