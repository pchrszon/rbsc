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

import qualified Rbsc.Syntax.Expr.Typed   as T
import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker


spec :: Spec
spec = describe "typeCheck" $ do
    it "computes the correct type" $
        typeCheck' TyBool [expr| n : N |]
        `shouldBe`
        Right (T.Variable "n" tyComponent `T.HasType` "N")

    it "detects type errors" $
        typeCheck' TyBool [expr| true : N |]
        `shouldSatisfy`
        has (_Left.Type._TypeError)

    it "detects undefined types" $
        typeCheck' TyBool [expr| n : Undefined |]
        `shouldSatisfy`
        has (_Left.Type._UndefinedType)

    it "detects undefined identifiers" $
        typeCheck' TyBool [expr| undefined : N |]
        `shouldSatisfy`
        has (_Left.Type._UndefinedIdentifier)


typeCheck' :: Type t -> Loc U.Expr -> Either Type.Error (T.Expr t)
typeCheck' ty e = do
    te <- typeCheck types symbolTable e
    extract ty (getLoc e) te


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
