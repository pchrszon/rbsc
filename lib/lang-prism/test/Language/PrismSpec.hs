{-# LANGUAGE OverloadedStrings #-}


module Language.PrismSpec (spec) where


import Common

import Language.Prism


spec :: Spec
spec = describe "pretty printer" $ do
    it "prints reward structure items without action" $
        RewardStructItem StateReward (LitBool True) 1 `shouldBe'` "true : 1;"
    it "prints reward structure items with action" $
        RewardStructItem (TransitionReward [Action "act"] ActionClosed)
        (LitBool True) 1
        `shouldBe'`
        "[act] true : 1;"
