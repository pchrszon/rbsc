{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.DependenciesSpec (spec) where


import Control.Arrow ((+++))

import Test.Hspec


import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

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
                function arr(i : int) : array n of int = f(i);
                function f(i : int) : int = i + n;
                function playerIn(p : component, c : compartment) : bool =
                    exists r : role. r in c & r boundto p;
                const k = arr(n)[0];
                const p = LOC;
                impl N {
                    x : enum { LOC };
                }
            |]
        `shouldBe`
        Right
            [ "LOC"
            , "n"
            , "sig_arr"
            , "sig_f"
            , "arr"
            , "f"
            , "k"
            , "p"
            , "N_x"
            , "N"
            , "sig_playerIn"
            , "playerIn"
            , "N_impl"
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
    idents <- toEither (identifierDefs m)
    (: []) +++ fmap getName $ sortDefinitions m idents


getName :: Dependency -> Name
getName = \case
    DepDefinition def -> case def of
        DefConstant c        -> unLoc (constName c)
        DefFunction f        -> unLoc (functionName f)
        DefLabel             -> "label"
        DefGlobal decl       -> unLoc (declName decl)
        DefLocal tyName _ decl ->
            getTypeName tyName <> "_" <> unLoc (declName decl)
        DefComponentType t -> getTypeName . unLoc $ case t of
            TypeDefNatural nt     -> ntdName nt
            TypeDefRole rt        -> rtdName rt
            TypeDefCompartment ct -> ctdName ct
        DefTypeSet s   -> getTypeName (unLoc (tsdName s))
        DefComponent c -> unLoc (compDefName c)
        DefModule m    -> unLoc (modName m)
    DepFunctionSignature f -> "sig_" <> unLoc (functionName f)
    DepModuleInstantiation mi -> unLoc (modName (midModule mi))
