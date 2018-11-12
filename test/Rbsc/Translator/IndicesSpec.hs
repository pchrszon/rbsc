{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.Translator.IndicesSpec (spec) where


import Control.Lens
import Control.Monad.Reader

import Test.Hspec


import Rbsc.Config

import Rbsc.Data.Component
import Rbsc.Data.Info
import Rbsc.Data.ModelInfo

import Rbsc.Instantiation

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Result

import qualified Rbsc.Syntax.Typed   as T
import qualified Rbsc.Syntax.Untyped as U

import Rbsc.Translator.Indices.Internal
import Rbsc.Translator.Instantiation

import Rbsc.TypeChecker


spec :: Spec
spec = describe "getIndexRanges" $
    it "retrieves the ranges of all variables used as an index" $
        getIndexRanges' testComponent testCommand
        `shouldBe`
        Right
            [ (Variable "b" (LocalVar "N" "n"), BoolRange)
            , (Variable "x" GlobalVar, IntRange (0, 1))
            , (Variable "y" (LocalVar "N" "n"), IntRange (0, 1))
            ]


testModel :: U.Model
testModel =
    [model|
        natural type N;

        system { n: N; }

        global x : [0 .. 1];

        impl N {
            arr : array 2 of bool;
            y : [0 .. 1];
            z : [0 .. 1];
            b : bool;

            [a[x]] arr[if b then 0 else 1] & z = 0 -> (arr[y]' = true);
        }
    |]


testComponent :: Component
testComponent = Component "n" "N" Nothing Nothing


testCommand :: T.TCommand T.Elem
testCommand = let T.ModuleBody _ [T.Elem cmd] = testModuleBody in cmd


testModuleBody :: T.TModuleBody T.Elem
testModuleBody =
    let Right [T.ModuleInstance _ _ r] =
            toEither . flip runReaderT (Info (view _2 typedTestModel) 10) $
                instantiateComponent
                    (view _1 typedTestModel)
                    testComponent
    in r


typedTestModel :: (T.Model, ModelInfo)
typedTestModel =
    let Right (model', [(_, info')]) =
            toEither . flip runReaderT (10 :: RecursionDepth) $ do
                (m', info) <- typeCheck testModel
                insts <- generateInstances m' info
                return (m', insts)
    in (model', info')


getIndexRanges' ::
       Component -> T.TCommand T.Elem -> Either [Error] [(Variable, Range)]
getIndexRanges' comp cmd =
    toEither . flip runReaderT (Info (view _2 typedTestModel) 10) $
        getIndexRanges T.cmdUpdates (Just comp) cmd
