{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.SymbolTableSpec (spec) where


import Control.Lens

import Test.Hspec

import qualified Rbsc.ComponentType       as CompTys
import           Rbsc.Parser.TH
import qualified Rbsc.Report.Error.Syntax as Syntax
import           Rbsc.SymbolTable
import           Rbsc.Syntax.Declaration
import           Rbsc.Type


spec :: Spec
spec = describe "fromDeclarations" $ do
    it "extracts all components" $
        fromDeclarations'
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
            [ ("n", AType (TyComponent (Just "N")))
            , ("r", AType (TyComponent (Just "R")))
            ]

    it "detects undefined types" $
        fromDeclarations'
            [model|
                system {
                    n : Undefined
                }
            |]
        `shouldSatisfy`
        has (_Left.traverse.Syntax._UndefinedType)

    it "detects duplicated components" $
        fromDeclarations'
            [model|
                natural type N;

                system {
                    n : N,
                    n : N
                }
            |]
        `shouldSatisfy`
        has (_Left.traverse.Syntax._DuplicateIdentifier)


fromDeclarations' :: [Declaration] -> Either [Syntax.Error] SymbolTable
fromDeclarations' decls = do
    types <- CompTys.fromDeclarations decls
    fromDeclarations types decls
