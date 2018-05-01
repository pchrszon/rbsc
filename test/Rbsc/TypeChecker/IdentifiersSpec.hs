{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.IdentifiersSpec (spec) where


import Control.Lens

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Test.Hspec


import Rbsc.Data.Scope

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker.Identifiers


spec :: Spec
spec = describe "identifierDefs" $ do
    it "extracts all identifiers" $
        identifierDefs
            [model|
                natural type N;

                const n : int = 5;

                function f(x : int) : int = x;

                global x : bool;

                system {
                    c : Comp
                }

                impl N {
                    x : bool;
                }
            |]
        `shouldBeLike`
        Right
            [ (ScopedName GlobalScope "N", DefComponentType
                (TypeDefNatural (NaturalTypeDef (dummyLoc "N"))))
            , (ScopedName GlobalScope "n", DefConstant
                (Constant (dummyLoc "n") (Just TyInt) (dummyLoc (LitInt 5))))
            , (ScopedName GlobalScope "f", DefFunction
                (Function
                    (dummyLoc "f")
                    [Parameter (dummyLoc "x") TyInt]
                    TyInt
                    (dummyLoc (Identifier "x"))))
            , (ScopedName GlobalScope "x", DefGlobal
                (VarDecl (dummyLoc "x") VarTyBool Nothing))
            , (ScopedName GlobalScope "c", DefComponent
                (ComponentDef
                    (dummyLoc "c")
                    (dummyLoc "Comp")
                    Nothing))
            , (ScopedName (LocalScope "N") "x", DefLocal "N"
                (VarDecl (dummyLoc "x") VarTyBool Nothing))
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
    -> Either [Error] (Map ScopedName IdentifierDef)
    -> Expectation
shouldBeLike x y = over _Right (Map.map unLoc) x `shouldBe` y


dummyLoc :: a -> Loc a
dummyLoc x = Loc x (Region "test" "" (Position 1 1) (Position 1 1))
