{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.EvalSpec (spec) where


import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Test.Hspec


import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region

import           Rbsc.Syntax.Expr.Typed   (Constants, SomeExpr (..))
import qualified Rbsc.Syntax.Expr.Typed   as T
import qualified Rbsc.Syntax.Expr.Untyped as U

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


constants :: Constants
constants = Map.fromList
    [ ("x", SomeExpr (T.Literal 1) TyInt)
    , ("n", SomeExpr
        (T.Literal (Component "n" "N" Nothing Nothing))
        (TyComponent (Set.singleton "N")))
    , ("r", SomeExpr
        (T.Literal (Component "r" "R" (Just "n") Nothing))
        (TyComponent (Set.singleton "R")))
    , ("f", SomeExpr -- f(i : int) : int = f(i)
        (T.Lambda TyInt (T.Scope (T.Apply
            (T.Identifier "f" (TyInt --> TyInt))
            (T.Bound 0 TyInt))))
        (TyInt --> TyInt))
    ]


symbolTable :: SymbolTable
symbolTable = Map.fromList
    [ ("x", SomeType TyInt)
    , ("y", SomeType TyInt)
    , ("n", SomeType (TyComponent (Set.singleton "N")))
    , ("r", SomeType (TyComponent (Set.singleton "R")))
    , ("f", SomeType (TyInt --> TyInt))
    ]

componentTypes :: ComponentTypes
componentTypes = Map.fromList
    [ ("N", NaturalType)
    , ("R", RoleType (Set.fromList ["N"]))
    ]


eval' :: Type t -> Loc U.Expr -> Either Error t
eval' ty e = do
    e' <-
        runTypeChecker (tcExpr e) componentTypes symbolTable >>=
        extract ty (getLoc e)
    eval constants 10 (e' `withLocOf` e)


reduce' :: Type t -> Loc U.Expr -> Either Error String
reduce' ty e = do
    e' <-
        runTypeChecker (tcExpr e) Map.empty symbolTable >>=
        extract ty (getLoc e)
    e'' <- reduce constants 10 (e' `withLocOf` e)
    return (show e'')
