{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.EvalSpec (spec) where


import Control.Lens

import Test.Hspec


import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Instancing

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region

import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker
import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal hiding (componentTypes, symbolTable)


spec :: Spec
spec = do
    describe "eval" $ do
        it "evaluates constant expressions" $
            eval' TyInt [expr| (x + 1) * 2 |]
            `shouldBe`
            Right 4

        it "evaluates quantified expressions" $
            eval' TyBool [expr| forall x : R. x boundto n |]
            `shouldBe`
            Right True

        it "evaluates functions containing quantified expressions" $
            eval' TyBool [expr| n playerIn c |]
            `shouldBe`
            Right True

        it "does short-circuit evaluation" $
            eval' TyInt [expr| if true | (1 / 0 > 0) then 1 else f(1) |]
            `shouldBe`
            Right 1

        it "detects division by zero" $
            eval' TyDouble [expr| 1.0 / 0 |]
            `shouldSatisfy`
            has (_Left.errorDesc._DivisionByZero)

        it "detects out-of-bound array accesses" $
            eval' TyInt [expr| {0, 1, 2}[3] |]
            `shouldSatisfy`
            has (_Left.errorDesc._IndexOutOfBounds)

        it "stops after exceeding the maximum recursion depth" $
            eval' TyInt [expr| f(1) |]
            `shouldSatisfy`
            has (_Left.errorDesc._ExceededDepth)

    describe "reduce" $ do
        it "evaluates constant expressions" $
            reduce' TyBool [expr| (x + 1) * 2 = 4 |]
            `shouldBe`
            Right "Literal True"

        it "evaluates constant sub-expressions" $
            reduce' TyBool [expr| y + 1 < x * 2 |]
            `shouldBe`
            Right "RelOp Lt (ArithOp Add (Identifier \"y\" TyInt) (Literal 1)) (Literal 2)"


modelInfo :: ModelInfo
modelInfo =
    let Right (model', info) = typeCheck 10
            [model|
                natural type N;
                role type R(N);
                compartment type C(R);

                const x: int = 1;

                function f(i: int) : int = f(i);

                function playerIn(p: component, c: compartment) : bool =
                    exists r: role. r in c & r boundto p;

                system {
                    n : N,
                    r : R,
                    c : C,
                    r boundto n,
                    r in c
                }
            |]
        Right [(_, info')] = generateInstances 10 model' info
    in info' & symbolTable.at "y" .~ Just (SomeType TyInt)


eval' :: Type t -> Loc U.Expr -> Either Error t
eval' ty e = do
    e' <-
        runTypeChecker
            (tcExpr e)
            (view componentTypes modelInfo)
            (view symbolTable modelInfo) >>=
            extract ty (getLoc e)
    eval (view constants modelInfo) 10 (e' `withLocOf` e)


reduce' :: Type t -> Loc U.Expr -> Either Error String
reduce' ty e = do
    e' <-
        runTypeChecker
            (tcExpr e)
            (view componentTypes modelInfo)
            (view symbolTable modelInfo) >>=
            extract ty (getLoc e)
    e'' <- reduce (view constants modelInfo) 10 (e' `withLocOf` e)
    return (show e'')
