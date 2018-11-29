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


import Util


spec :: Spec
spec = describe "identifierDefs" $ do
    it "extracts all identifiers" $
        identifierDefs
            [model|
                natural type N;
                natural type K;

                const n : int = 5;

                function f(x : int) : int = x;

                global x : bool;

                system {
                    c : Comp;
                }

                impl N {
                    x : enum { FIRST, SECOND };
                }

                impl K {
                    y : bool;
                }

                coordinator {
                    z : bool;
                }
            |]
        `shouldBeLike`
        Right
            [ (ScopedName Global "N", DefComponentType
                (TypeDefNatural (NaturalTypeDef (dummyLoc "N"))))
            , (ScopedName Global "K", DefComponentType
                (TypeDefNatural (NaturalTypeDef (dummyLoc "K"))))
            , (ScopedName Global "N_impl", DefModule
                (Module (dummyLoc "N_impl") []
                    (ModuleBody
                        [ VarDecl (dummyLoc "x")
                            (VarTyEnum (Enumeration [dummyLoc "FIRST", dummyLoc "SECOND"]))
                            Nothing
                        ] [])))
            , (ScopedName Global "K_impl", DefModule
                (Module (dummyLoc "K_impl") [] (ModuleBody [VarDecl (dummyLoc "y") VarTyBool Nothing] [])))
            , (ScopedName Global "n", DefConstant
                (Constant (dummyLoc "n") (Just TyInt) (dummyLoc (LitInt 5))))
            , (ScopedName Global "f", DefFunction
                (Function
                    Nothing
                    (dummyLoc "f")
                    [Parameter (dummyLoc "x") TyInt]
                    TyInt
                    (dummyLoc (Identifier "x"))))
            , (ScopedName Global "x", DefGlobal
                (VarDecl (dummyLoc "x") VarTyBool Nothing))
            , (ScopedName Global "FIRST", DefConstant
                (Constant (dummyLoc "FIRST") (Just TyInt) (dummyLoc (LitInt 0))))
            , (ScopedName Global "SECOND", DefConstant (Constant
                (dummyLoc "SECOND") (Just TyInt) (dummyLoc (LitInt 1))))
            , (ScopedName Global "c", DefComponent
                (ComponentDef
                    (dummyLoc "c")
                    (dummyLoc "Comp")
                    Nothing))
            , (ScopedName (Local "N") "x", DefLocal "N" "N_impl"
                (VarDecl (dummyLoc "x") (VarTyEnum (Enumeration [dummyLoc "FIRST", dummyLoc "SECOND"])) Nothing))
            , (ScopedName (Local "K") "y", DefLocal "K" "K_impl"
                (VarDecl (dummyLoc "y") VarTyBool Nothing))
            , (ScopedName Global "z", DefGlobal
                (VarDecl (dummyLoc "z") VarTyBool Nothing))
            ]

    it "detects duplicated identifiers" $
        identifierDefs
            [model|
                const n : int = 5;

                system {
                    n : Comp;
                }
            |]
        `shouldThrowError`
        _DuplicateIdentifier


shouldBeLike ::
       Either [Error] Identifiers
    -> Either [Error] (Map ScopedName IdentifierDef)
    -> Expectation
shouldBeLike x y = over _Right (Map.map unLoc) x `shouldBe` y


dummyLoc :: a -> Loc a
dummyLoc x = Loc x (Region "test" "" (Position 1 1) (Position 1 1))
