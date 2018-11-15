{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.EvalSpec (spec) where


import Control.Lens
import Control.Monad.Reader

import Test.Hspec


import Rbsc.Config

import Rbsc.Data.Action
import Rbsc.Data.Info
import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Instantiation

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result (toEither)

import           Rbsc.Syntax.Untyped      (Model)
import qualified Rbsc.Syntax.Untyped.Expr as U

import Rbsc.TypeChecker
import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal hiding (componentTypes, symbolTable)


import Util


spec :: Spec
spec = do
    describe "eval" $ do
        it "evaluates constant expressions" $
            eval' TyInt [expr| (x + 1) * 2 |]
            `shouldBe`
            Right 4

        it "evaluates quantified expressions over components" $
            eval' TyBool [expr| exists x : R. x boundto n |]
            `shouldBe`
            Right True

        it "evaluates quantified expressions over integers" $
            eval' TyBool [expr| forall i : [0..2]. i < 3 |]
            `shouldBe`
            Right True

        it "evaluates sum expressions" $
            eval' TyInt [expr| sum i : [1..3]. i |]
            `shouldBe`
            Right 6

        it "evaluates nested quantified expressions" $
            eval' TyBool
                [expr|
                    forall x : component, r1 : role, r2 : role.
                        r1 boundto x & r2 boundto x => r1 = r2
                |]
            `shouldBe`
            Right True

        it "evaluates functions containing quantified expressions over components" $
            eval' TyBool [expr| n playerin c |]
            `shouldBe`
            Right True

        it "evaluates functions containing quantified expressions over integers" $
            eval' TyBool [expr| has_zero(square, -2, 2) |]
            `shouldBe`
            Right True

        it "evaluates built-in functions" $
            eval' TyInt [expr| min(2, x) |]
            `shouldBe`
            Right 1

        it "evaluates the count function" $
            eval' TyInt [expr| count(R, c) |]
            `shouldBe`
            Right 1

        it "evaluates the length function" $
            eval' TyInt [expr| length(arr) |]
            `shouldBe`
            Right 3

        it "evaluates the has_player function" $
            eval' TyBool [expr| has_player(r) |]
            `shouldBe`
            Right True

        it "evaluates the player function" $
            eval' TyBool [expr| player(r) = n |]
            `shouldBe`
            Right True

        it "evaluates the player function for indexed players" $
            eval' TyBool [expr| player(r_arr[1]) = n_arr[1] |]
            `shouldBe`
            Right True

        it "throws an error when calling player on a non-role" $
            eval' TyBool
                [expr| forall c. player(c) = n |]
            `shouldThrowError`
            _HasNoPlayer

        it "evaluates the index function" $
            eval' TyInt [expr| index(n_arr[1]) |]
            `shouldBe`
            Right 1

        it "throws an error when calling index on a non-indexed component" $
            eval' TyInt [expr| index(n) |]
            `shouldThrowError`
            _NonIndexedComponent

        it "does short-circuit evaluation" $
            eval' TyInt [expr| if true | (1 / 0 > 0) then 1 else f(1) |]
            `shouldBe`
            Right 1

        it "evaluates actions" $
            evalAct [expr| a |]
            `shouldBe`
            Right (Action "a")

        it "evaluates local actions" $
            evalAct [expr| n.a |]
            `shouldBe`
            Right (LocalAction "n" "a")

        it "evaluates indexed actions" $
            evalAct [expr| n.a[2] |]
            `shouldBe`
            Right (IndexedAction (LocalAction "n" "a") 2)

        it "detects division by zero" $
            eval' TyDouble [expr| 1.0 / 0 |]
            `shouldThrowError`
            _DivisionByZero

        it "detects out-of-bound array accesses" $
            eval' TyInt [expr| [0, 1, 2][3] |]
            `shouldThrowError`
            _IndexOutOfBounds

        it "stops after exceeding the maximum recursion depth" $
            eval' TyInt [expr| f(1) |]
            `shouldThrowError`
            _ExceededDepth

    describe "reduce" $ do
        it "evaluates constant expressions" $
            reduce' TyBool [expr| (x + 1) * 2 = 4 |]
            `shouldBe`
            Right "Literal True TyBool"

        it "evaluates constant sub-expressions" $
            reduce' TyBool [expr| y + 1 < x * 2 |]
            `shouldBe`
            Right "RelOp Lt (ArithOp Add (Identifier \"y\" TyInt) (Literal 1 TyInt)) (Literal 2 TyInt)"

        it "resolves array accesses for non-constant arrays" $
            reduce' TyInt [expr| [y, z][1] |]
            `shouldBe`
            Right "Identifier \"z\" TyInt"


testModel :: Model
testModel =
    [model|
        natural type N;
        role type R(N);
        compartment type C(R);

        const x : int = 1;

        global y : [0..1];

        global z : [0..1];

        global arr : array 3 of bool;

        function f(i : int) : int = f(i);

        function square(x : int) : int = x * x;

        function has_zero(f : int -> int, from: int, to: int) : bool =
            exists x: [from .. to]. f(x) = 0;

        function playerin(p : component, c : compartment) : bool =
            exists r: role. r in c & r boundto p;

        system {
            n : N;
            n_arr[2] : N;
            r : R;
            r_arr[2] : R;
            c : C;
            r boundto n;
            forall i : [0 .. 1]. r_arr[i] boundto n_arr[i];
            r in c;
        }
    |]


testModelInfo :: ModelInfo
testModelInfo =
    let Right [(_, info')] =
            toEither . flip runReaderT (10 :: RecursionDepth) $ do
                (model', info) <- typeCheck testModel
                generateInstances model' info
    in info'


eval' :: Type t -> Loc U.Expr -> Either [Error] t
eval' ty e = do
    e' <- toEither
        (runTypeChecker (tcExpr e) testModelInfo 10 >>= extract ty (getLoc e))
    over _Left
         (: [])
         (runReaderT (eval (e' `withLocOf` e)) (Info testModelInfo 10))


evalAct :: Loc U.Expr -> Either [Error] Action
evalAct e = do
    e' <- toEither
        (   runTypeChecker (tcAction e) testModelInfo 10
        >>= extract TyAction (getLoc e)
        )
    over _Left
         (: [])
         (runReaderT (eval (e' `withLocOf` e)) (Info testModelInfo 10))


reduce' :: Type t -> Loc U.Expr -> Either [Error] String
reduce' ty e = do
    e' <- toEither
        (runTypeChecker (tcExpr e) testModelInfo 10 >>= extract ty (getLoc e))
    e'' <- over
        _Left
        (: [])
        (runReaderT (reduce (e' `withLocOf` e)) (Info testModelInfo 10))
    return (show e'')
