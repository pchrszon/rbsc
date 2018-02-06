{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.EvalSpec (spec) where


import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Test.Hspec


import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.SymbolTable
import Rbsc.Data.Type
import Rbsc.Data.Value

import Rbsc.Eval

import Rbsc.Parser.TH

import qualified Rbsc.Report.Error.Eval as Eval
import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region     (Loc (..))

import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker


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

        it "detects division by zero" $
            eval' TyDouble [expr| 1.0 / 0 |]
            `shouldSatisfy`
            has (_Left._Right.Eval._DivisionByZero)

    describe "reduce" $ do
        it "evaluates constant expressions" $
            reduce' TyBool [expr| (x + 1) * 2 = 4 |]
            `shouldBe`
            Right "Literal True"

        it "evaluates constant sub-expressions" $
            reduce' TyBool [expr| y + 1 < x * 2 |]
            `shouldBe`
            Right "RelOp Lt (ArithOp Add (Variable \"y\" TyInt) (Literal 1)) (Literal 2)"


constants :: Constants
constants = Map.fromList
    [ ("x", Value 1 TyInt)
    , ("n", Value (Component "n" "N" Nothing Nothing) (TyComponent (Just "N")))
    , ("r", Value
        (Component "r" "R" (Just "n") Nothing)
        (TyComponent (Just "R")))
    ]


symbolTable :: SymbolTable
symbolTable = Map.fromList
    [ ("x", SomeType TyInt)
    , ("y", SomeType TyInt)
    , ("n", SomeType (TyComponent (Just "N")))
    , ("r", SomeType (TyComponent (Just "R")))
    ]

componentTypes :: ComponentTypes
componentTypes = Map.fromList
    [ ("N", NaturalType)
    , ("R", RoleType (Set.fromList ["N"]))
    ]


eval' :: Type t -> Loc U.Expr -> Either (Either Type.Error Eval.Error) t
eval' ty e =
    case typeCheck componentTypes symbolTable e >>= extract ty (getLoc e) of
        Left err -> Left (Left err)
        Right e' -> case eval constants 100 (Loc e' (getLoc e)) of
            Left err -> Left (Right err)
            Right x  -> Right x


reduce' ::
       Type t -> Loc U.Expr -> Either (Either Type.Error Eval.Error) String
reduce' ty e =
    case typeCheck Map.empty symbolTable e >>= extract ty (getLoc e) of
        Left err -> Left (Left err)
        Right e' -> case reduce constants 100 (Loc e' (getLoc e)) of
            Left err  -> Left (Right err)
            Right e'' -> Right (show e'')
