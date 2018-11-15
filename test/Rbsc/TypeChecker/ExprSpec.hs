{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.ExprSpec (spec) where


import Control.Lens
import Control.Monad.Reader

import Test.Hspec


import Rbsc.Config

import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Untyped      (Model)
import qualified Rbsc.Syntax.Untyped.Expr as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal  (extract, runTypeChecker)
import Rbsc.TypeChecker.ModelInfo


import Util


spec :: Spec
spec = describe "typeCheck" $ do
    it "computes the correct type" $
        typeCheck TyBool [expr| n : N |]
        `shouldBe`
        Right "HasType (Identifier \"n\" (TyComponent (fromList [\"N\"]))) \"N\""

    it "detects type errors" $
        typeCheck TyBool [expr| true : N |]
        `shouldThrowError`
        _TypeError

    it "reports comparison of uncomparable values" $
        typeCheck TyBool [expr| n > 5 |]
        `shouldThrowError`
        _NotComparable

    it "reports undefined local variables" $
        typeCheck TyInt [expr| n.y |]
        `shouldThrowError`
        _UndefinedMember

    it "reports conflicting local variable types" $
        typeCheck TyInt [expr| forall c : {N, K}. c.x > 0 |]
        `shouldThrowError`
        _ConflictingMemberTypes

    it "reports use of the self keyword in global scope" $
        typeCheck TyInt [expr| self.x |]
        `shouldThrowError`
        _SelfOutsideImpl

    it "reports indexing of non-array values" $
        typeCheck TyBool [expr| n[1] |]
        `shouldThrowError`
        _NotAnArray

    it "reports invocation on a non-function value" $
        typeCheck TyBool [expr| n(1) |]
        `shouldThrowError`
        _NotAFunction

    it "detects too many arguments on a function call" $
        typeCheck TyBool [expr| floor(1, 2) |]
        `shouldThrowError`
        _WrongNumberOfArguments

    it "detects undefined types" $
        typeCheck TyBool [expr| n : Undefined |]
        `shouldThrowError`
        _UndefinedType

    it "detects undefined identifiers" $
        typeCheck TyBool [expr| undefined : N |]
        `shouldThrowError`
        _UndefinedIdentifier


testModel :: Model
testModel =
    [model|
        natural type N;
        natural type K;

        system { n : N; k : K; }

        impl N {
            x : bool;
        }

        impl K {
            x : [0..1];
        }
    |]


typeCheck :: Type t -> Loc U.Expr -> Either [Error] String
typeCheck ty e = toEither $ do
    te <- runTypeChecker
            (tcExpr e)
            (view componentTypes modelInfo)
            (view typeSets modelInfo)
            (view symbolTable modelInfo)
            (view constants modelInfo)
            10
    show <$> extract ty (getLoc e) te


modelInfo :: ModelInfo
modelInfo =
    let Right (info, _) = toEither
            (runReaderT (getModelInfo testModel) (10 :: RecursionDepth))
    in  info
