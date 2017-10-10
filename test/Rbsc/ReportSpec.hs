{-# LANGUAGE OverloadedStrings #-}


module Rbsc.ReportSpec (spec) where


import Data.Monoid
import Data.Text (Text)

import Test.Hspec

import Rbsc.Report
import Rbsc.Report.Region


spec :: Spec
spec = describe "render" $ do
    it "underlines the region" $
        let s = Region "test" testSource (Position 2 1) (Position 2 7)
            msg = "test report message"
        in shouldBe (show (render s msg)) $
            "test:2:1:\n" <>
            "test report message\n" <>
            "  |\n" <>
            "2 | second line\n" <>
            "  | ^^^^^^\n"
    it "handles multi-line regions" $
        let s = Region "test" testSource (Position 1 7) (Position 3 6)
            msg = "test report message"
        in shouldBe (show (render s msg)) $
            "test:1:7:\n" <>
            "test report message\n" <>
            "  |\n" <>
            "1 | first line\n" <>
            "  |       ^^^^\n" <>
            "2 | second line\n" <>
            "  | ^^^^^^^^^^^\n" <>
            "3 | third line\n" <>
            "  | ^^^^^\n"



testSource :: Text
testSource =
    "first line\n" <>
    "second line\n" <>
    "third line\n"
