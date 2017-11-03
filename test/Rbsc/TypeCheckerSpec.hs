{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeCheckerSpec (spec) where


import Control.Lens

import qualified Data.Map.Strict as Map

import Test.Hspec


import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.SymbolTable
import Rbsc.Data.Type

import Rbsc.Parser.TH

import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region

import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker


spec :: Spec
spec = describe "typeCheck" $ do
    it "computes the correct type" $
        typeCheck' TyBool [expr| n : N |]
        `shouldBe`
        Right "HasType (Variable \"n\" (TyComponent Nothing)) (TypeName {getTypeName = \"N\"})"

    it "detects type errors" $
        typeCheck' TyBool [expr| true : N |]
        `shouldSatisfy`
        has (_Left.Type._TypeError)

    it "reports comparison of uncomparable values" $
        typeCheck' TyBool [expr| n > 5 |]
        `shouldSatisfy`
        has (_Left.Type._NotComparable)

    it "reports indexing of non-array values" $
        typeCheck' TyBool [expr| n[1] |]
        `shouldSatisfy`
        has (_Left.Type._NotAnArray)

    it "reports invocation on a non-function value" $
        typeCheck' TyBool [expr| n(1) |]
        `shouldSatisfy`
        has (_Left.Type._NotAFunction)

    it "detects too many arguments on a function call" $
        typeCheck' TyBool [expr| min(1, 2, 3) |]
        `shouldSatisfy`
        has (_Left.Type._WrongNumberOfArguments)

    it "detects undefined types" $
        typeCheck' TyBool [expr| n : Undefined |]
        `shouldSatisfy`
        has (_Left.Type._UndefinedType)

    it "detects undefined identifiers" $
        typeCheck' TyBool [expr| undefined : N |]
        `shouldSatisfy`
        has (_Left.Type._UndefinedIdentifier)


typeCheck' :: Type t -> Loc U.Expr -> Either Type.Error String
typeCheck' ty e = do
    te <- typeCheck types symbolTable e
    show <$> extract ty (getLoc e) te


types :: ComponentTypes
types = Map.fromList
    [ ("N", NaturalType)
    ]

symbolTable :: SymbolTable
symbolTable = Map.fromList
    [ ("n", AType tyComponent)
    ]

tyComponent :: Type Component
tyComponent = TyComponent Nothing
