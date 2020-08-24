{-# LANGUAGE OverloadedStrings #-}


module Language.Prism.DeclarationSpec (spec) where


import Common

import Language.Prism.Declaration
import Language.Prism.Expression


spec :: Spec
spec = describe "pretty printer" $ do
    it "prints declarations without initialization" $
        Declaration "x" (DeclTypeInt 0 5) Nothing
        `shouldBe'`
        "x : [0..5];"
    it "prints declarations with initialization" $
        Declaration "x" DeclTypeBool (Just (LitBool True))
        `shouldBe'`
        "x : bool init true;"
