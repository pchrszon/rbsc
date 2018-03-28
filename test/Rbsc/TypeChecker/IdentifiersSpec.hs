{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.IdentifiersSpec (spec) where


import Control.Lens

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Test.Hspec


import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker.Identifiers


spec :: Spec
spec = describe "identifierDefs" $ do
    it "extracts all constants, functions and components" $
        identifierDefs
            [model|
                const n : int = 5;

                function f(x : int) : int = x;

                system {
                    c : Comp
                }
            |]
        `shouldBeLike`
        Right
            [ ("n", DefConstant
                (Constant (dummyLoc "n") (Just TyInt) (dummyLoc (LitInt 5))))
            , ("f", DefFunction
                (Function
                    (dummyLoc "f")
                    [Parameter (dummyLoc "x") TyInt]
                    TyInt
                    (dummyLoc (Identifier "x"))))

            , ("c", DefComponent
                (ComponentDef
                    (dummyLoc "c")
                    (dummyLoc "Comp")
                    Nothing))
            ]

    it "detects duplicated identifiers" $
        identifierDefs
            [model|
                const n : int = 5;

                system {
                    n : Comp
                }
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._DuplicateIdentifier)


shouldBeLike ::
       Either [Error] Identifiers
    -> Either [Error] (Map Name IdentifierDef)
    -> Expectation
shouldBeLike x y = over _Right (Map.map unLoc) x `shouldBe` y


dummyLoc :: a -> Loc a
dummyLoc x = Loc x (Region "test" "" (Position 1 1) (Position 1 1))
