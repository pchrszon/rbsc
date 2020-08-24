{-# LANGUAGE OverloadedStrings #-}


module Language.Prism.ExpressionSpec (spec) where


import Common

import Language.Prism.Expression
import Language.Prism.Functions
import Language.Prism.Operators


spec :: Spec
spec = describe "pretty printer" $ do
    it "adds parentheses if necessary" $
        (2 * (3 + 4) :: Expr) `shouldBe'` "2 * (3 + 4)"
    it "does not add parentheses if inner operator binds stronger" $
        (2 + 3 * 4 :: Expr) `shouldBe'` "2 + 3 * 4"
    it "does not add parentheses if inner operator binds equally strong" $
        (2 + 3 + 4 :: Expr) `shouldBe'` "2 + 3 + 4"
    it "handles unary operators correctly" $
        UnaryOp Not (BinaryOp (Ident "x") And (Ident "y"))
        `shouldBe'`
        "!(x & y)"
    it "prints functions correctly" $
        Func FuncMax [1, 2, 3] `shouldBe'` "max(1, 2, 3)"
    it "handles P operator with bound" $
        Prob RelGt (Just 0) (Ident "x") `shouldBe'` "P>0 [ x ]"
    it "handles P query" $
        Prob RelMax Nothing (Ident "x") `shouldBe'` "Pmax=? [ x ]"
    it "handles R operator with reward structure" $
        Reward (Just "r") RelGt (Just 5) (Ident "x") `shouldBe'`
        "R{\"r\"}>5 [ x ]"
    it "prints time bounds on temporal operator" $
        Temporal
            Nothing
            Finally
            (Just (Ident "x"))
            (Just 5)
            Inclusive
            Nothing
            Inclusive
        `shouldBe'`
        "F<=5 x"
    it "adds spaces arounf temporal operator" $
        Temporal
             (Just (Ident "x"))
             Until
             (Just (Ident "y"))
             Nothing
             Inclusive
             Nothing
             Inclusive
        `shouldBe'`
        "x U y"
    it "prints filters correctly" $
        Filter FilterPrint (Ident "x") (LitLabel "init")
        `shouldBe'`
        "filter(print, x, \"init\")"
