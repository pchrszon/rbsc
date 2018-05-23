{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.TypeChecker.ComponentTypesSpec (spec) where


import Control.Lens
import Control.Monad.Reader

import Test.Hspec


import Rbsc.Config

import Rbsc.Data.ModelInfo

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Result

import Rbsc.Syntax.Typed   (TConstant)
import Rbsc.Syntax.Untyped (Model)

import Rbsc.TypeChecker.ModelInfo


spec :: Spec
spec = describe "validateComponentTypes" $ do
    it "detects undefined types" $
        getModelInfo'
            [model|
                role type R(Undefined);
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._UndefinedType)

    it "detects non-role types in compartments" $
        getModelInfo'
            [model|
                natural type N;
                compartment type C(N);
            |]
        `shouldSatisfy`
        has (_Left.traverse.errorDesc._NonRoleInCompartment)


getModelInfo' :: Model -> Either [Error] (ModelInfo, [TConstant])
getModelInfo' m = toEither (runReaderT (getModelInfo m) (10 :: RecursionDepth))
