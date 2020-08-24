{-# LANGUAGE OverloadedStrings #-}


module Language.Prism.CommandSpec (spec) where


import Common

import Language.Prism.Command
import Language.Prism.Expression


spec :: Spec
spec = describe "pretty printer" $ do
    it "prints empty assignments" $
        Update Nothing [] `shouldBe'` "true"
    it "prints assignments without probability" $
        Update Nothing [("x", 1)] `shouldBe'` "(x' = 1)"
    it "prints assignments with probability" $
        Update (Just (LitDouble 0.5)) [("x", 1)] `shouldBe'` "0.5:(x' = 1)"
    it "prints multiple assignments" $
        Update Nothing [("x", 1), ("y", 2)] `shouldBe'` "(x' = 1) & (y' = 2)"
    it "prints empty updates" $
        Command [] ActionClosed (LitBool True) [] `shouldBe'` "[] true -> true;"
    it "prints commands" $
        Command [Action "action"] ActionClosed (LitBool True)
            [Update Nothing [("x", 1)]]
        `shouldBe'`
        "[action] true -> (x' = 1);"
    it "prints open multi-actions" $
        Command [Action "a", Action "b"] ActionOpen (LitBool True) []
        `shouldBe'`
        "]a, b[ true -> true;"
