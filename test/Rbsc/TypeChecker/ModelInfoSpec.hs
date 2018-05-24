{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.ModelInfoSpec (spec) where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader

import Test.Hspec


import Rbsc.Config

import Rbsc.Data.ModelInfo
import Rbsc.Data.Scope

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Result

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker.ModelInfo


spec :: Spec
spec = describe "getModelInfo" $
    it "computes the variable ranges" $
        fmap _miRangeTable (getModelInfo'
            [model|
                natural type N;

                global g : [0 .. 4];

                impl N {
                    x : [0 .. 1];
                    arr : array [0 .. 2] of [0 .. 3];
                    y : bool;
                }
            |])
        `shouldBe`
        Right (Map.fromList
            [ (ScopedName Global "g", (0, 4))
            , (ScopedName (Local "N") "x", (0, 1))
            , (ScopedName (Local "N") "arr", (0, 3))
            ])


getModelInfo' :: Model -> Either [Error] ModelInfo
getModelInfo' m =
    fst <$> toEither (runReaderT (getModelInfo m) (10 :: RecursionDepth))
