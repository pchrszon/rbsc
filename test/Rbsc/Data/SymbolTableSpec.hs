{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.Data.SymbolTableSpec (spec) where


import Control.Lens

import Test.Hspec

import qualified Rbsc.Data.ComponentType as CompTys
import           Rbsc.Data.SymbolTable
import           Rbsc.Data.Type

import Rbsc.Parser.TH

import qualified Rbsc.Report.Error.Syntax as Syntax

import Rbsc.Syntax.Untyped


spec :: Spec
spec = describe "fromModel" $ do
    it "extracts all components" $
        fromModel'
            [model|
                natural type N;
                role type R(N);

                system {
                    n : N,
                    r : R
                }
            |]
        `shouldBe`
        Right
            [ ("n", SomeType (TyComponent (Just "N")))
            , ("r", SomeType (TyComponent (Just "R")))
            ]

    it "detects undefined types" $
        fromModel'
            [model|
                system {
                    n : Undefined
                }
            |]
        `shouldSatisfy`
        has (_Left.traverse.Syntax._UndefinedType)

    it "detects duplicated components" $
        fromModel'
            [model|
                natural type N;

                const n : int = 1;

                system {
                    n : N
                }
            |]
        `shouldSatisfy`
        has (_Left.traverse.Syntax._DuplicateIdentifier)


fromModel' :: UModel -> Either [Syntax.Error] SymbolTable
fromModel' m = do
    types <- CompTys.fromModel m
    fromModel types m
