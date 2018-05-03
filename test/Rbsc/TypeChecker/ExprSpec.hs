{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.ExprSpec (spec) where


import Control.Lens

import Test.Hspec


import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal (extract, runTypeChecker)
import Rbsc.TypeChecker.ModelInfo


spec :: Spec
spec = describe "typeCheck" $ do
    it "computes the correct type" $
        typeCheck TyBool [expr| n : N |]
        `shouldBe`
        Right "HasType (Identifier \"n\" (TyComponent (fromList [\"N\"]))) \"N\""

    it "detects type errors" $
        typeCheck TyBool [expr| true : N |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._TypeError)

    it "reports comparison of uncomparable values" $
        typeCheck TyBool [expr| n > 5 |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._NotComparable)

    it "reports undefined local variables" $
        typeCheck TyInt [expr| n.y |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._UndefinedMember)

    it "reports conflicting local variable types" $
        typeCheck TyInt [expr| forall c: {N, K}. c.x > 0 |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._ConflictingMemberTypes)

    it "reports use of the self keyword in global scope" $
        typeCheck TyInt [expr| self.x |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._SelfOutsideImpl)

    it "reports indexing of non-array values" $
        typeCheck TyBool [expr| n[1] |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._NotAnArray)

    it "reports invocation on a non-function value" $
        typeCheck TyBool [expr| n(1) |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._NotAFunction)

    it "detects too many arguments on a function call" $
        typeCheck TyBool [expr| floor(1, 2) |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._WrongNumberOfArguments)

    it "detects undefined types" $
        typeCheck TyBool [expr| n : Undefined |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._UndefinedType)

    it "detects undefined identifiers" $
        typeCheck TyBool [expr| undefined : N |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._UndefinedIdentifier)


typeCheck :: Type t -> Loc U.Expr -> Either [Error] String
typeCheck ty e = toEither $ do
    te <- runTypeChecker
            (tcExpr e)
            (view componentTypes modelInfo)
            (view symbolTable modelInfo)
    show <$> extract ty (getLoc e) te


modelInfo :: ModelInfo
modelInfo =
    let Right (info, _) = toEither . getModelInfo 10 $
            [model|
                natural type N;
                natural type K;

                system { n: N, k: K }

                impl N {
                    x : bool;
                }

                impl K {
                    x : [0..1];
                }
            |]
    in info
