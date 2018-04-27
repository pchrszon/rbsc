{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.ExprSpec (spec) where


import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Test.Hspec


import Rbsc.Data.ComponentType
import Rbsc.Data.Type

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal (extract, runTypeChecker)


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
    te <- runTypeChecker (tcExpr e) types symbolTable
    show <$> extract ty (getLoc e) te


types :: ComponentTypes
types = Map.fromList
    [ ("N", NaturalType)
    ]

symbolTable :: SymbolTable
symbolTable = Map.fromList
    [ ((GlobalScope, "n"), SomeType (TyComponent (Set.singleton "N")))
    ]
