{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.DependenciesSpec (spec) where


import Control.Arrow ((+++))

import Test.Hspec


import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker.Dependencies
import Rbsc.TypeChecker.Identifiers


import Util


spec :: Spec
spec = describe "sortDefinitions" $ do
    it "finds a correct ordering of definitions" $
        dependencies
            [model|
                natural type N;
                const n = 2;
                function arr(i : int) : array [0..n] of int = f(i);
                function f(i : int) : int = i + n;
                function playerIn(p: component, c: compartment) : bool =
                    exists r: role. r in c & r boundto p;
                const k = arr(n)[0];
                impl N {
                    x : bool;
                }
            |]
        `shouldBe`
        Right
            [ "n"
            , "sig_arr"
            , "sig_f"
            , "arr"
            , "f"
            , "k"
            , "N_x"
            , "N"
            , "sig_playerIn"
            , "playerIn"
            ]

    it "detects cyclic definitions" $
        dependencies
            [model|
                const n = k + 1;
                const k = n + 1;
            |]
        `shouldThrowError`
        _CyclicDefinition


dependencies :: Model -> Either [Error] [Name]
dependencies m = do
    idents <- identifierDefs m
    (: []) +++ fmap getName $ sortDefinitions idents


getName :: Dependency -> Name
getName = \case
    DepDefinition def -> case def of
        DefConstant c        -> unLoc (constName c)
        DefFunction f        -> unLoc (functionName f)
        DefGlobal decl       -> unLoc (declName decl)
        DefLocal tyName decl ->
            getTypeName tyName <> "_" <> unLoc (declName decl)
        DefComponentType t -> getTypeName . unLoc $ case t of
            TypeDefNatural nt     -> ntdName nt
            TypeDefRole rt        -> rtdName rt
            TypeDefCompartment ct -> ctdName ct
        DefComponent c -> unLoc (compDefName c)
    DepFunctionSignature f -> "sig_" <> unLoc (functionName f)
