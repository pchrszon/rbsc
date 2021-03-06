{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.InstantiationSpec (spec) where


import Control.Lens
import Control.Monad.Reader

import Data.Foldable
import Data.Maybe

import Test.Hspec

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Rbsc.Config

import Rbsc.Data.Component
import Rbsc.Data.Field
import Rbsc.Data.ModelInfo
import Rbsc.Data.Name
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Instantiation.Internal

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Result

import Rbsc.Syntax.Typed.Expr (Expr (..), SomeExpr (..))
import Rbsc.Syntax.Untyped    (Model)

import Rbsc.TypeChecker


import Util


spec :: Spec
spec = do
    describe "buildSystem" $ do
        it "extracts instances, boundto and in relations" $
            buildSystem' simpleModel
            `shouldBe`
            Right System
                { _instances =
                    [ ("n", "N")
                    , ("r", "R")
                    , ("c", "C")
                    ]
                , _boundTo = [("r", "n")]
                , _containedIn = [("r", "c")]
                }

        it "handles component arrays" $
            buildSystem'
                [model|
                    natural type N;
                    role type R(N);
                    system {
                        n[2] : N;
                        r : R;
                        r boundto n[1];
                    }
                |]
            `shouldBe`
            Right System
                { _instances =
                    [ (ComponentName "n" (Just 0), "N")
                    , (ComponentName "n" (Just 1), "N")
                    , ("r", "R")
                    ]
                , _boundTo = [("r", ComponentName "n" (Just 1))]
                , _containedIn = []
                }

        it "handles quantification" $
            buildSystem'
                [model|
                    natural type N;
                    role type R(N);

                    const SIZE = 2;

                    system {
                        n[SIZE] : N;
                        r[SIZE] : R;
                        forall i : [0 .. SIZE - 1]. r[i] boundto n[i];
                    }
                |]
            `shouldBe`
            Right System
                { _instances =
                    [ (ComponentName "n" (Just 0), "N")
                    , (ComponentName "n" (Just 1), "N")
                    , (ComponentName "r" (Just 0), "R")
                    , (ComponentName "r" (Just 1), "R")
                    ]
                , _boundTo =
                    [ (ComponentName "r" (Just 0), ComponentName "n" (Just 0))
                    , (ComponentName "r" (Just 1), ComponentName "n" (Just 1))
                    ]
                , _containedIn = []
                }

        it "detects invalid use of 'boundto' relation" $
            buildSystem'
                [model|
                    natural type N;
                    natural type M;
                    system {
                        n : N;
                        m : M;
                        n boundto m;
                    }
                |]
            `shouldThrowError`
            _NotARole

        it "detects invalid bindings" $
            buildSystem'
                [model|
                    natural type N;
                    natural type M;
                    role type R(N);
                    system {
                        m : M;
                        r : R;
                        r boundto m;
                    }
                |]
            `shouldThrowError`
            _InvalidBinding

        it "detects multiple bindings of the same role" $
            buildSystem'
                [model|
                    natural type N;
                    role type R(N);
                    system {
                        n : N;
                        m : N;
                        r : R;
                        r boundto n;
                        r boundto m;
                    }
                |]
            `shouldThrowError`
            _RoleAlreadyBound

        it "detects invalid use of 'in' relation" $
            buildSystem'
                [model|
                    natural type N;
                    role type R(N);
                    system {
                        n : N;
                        r : R;
                        r in n;
                    }
                |]
            `shouldThrowError`
            _NotACompartment


    describe "checkCompartmentUpperBounds" $
        it "detects overfull compartments" $
            checkCompartmentUpperBounds'
                [model|
                    natural type N;
                    role type R(N);
                    compartment type C(R);

                    system {
                        r[2] : R;
                        c : C;
                        r[0] in c;
                        r[1] in c;
                    }
                |]
            `shouldBe`
            Left [NoLocError (TooManyRoles "c" [("R", 1)])]


    describe "updateModelInfo" $ do
        it "creates constants for components" $
            getComponents simpleModel
            `shouldBe`
            Right
                [ ("n", Component "n" "N" Nothing Nothing)
                , ("r", Component "r" "R" (Just "n") (Just "c"))
                , ("c", Component "c" "C" Nothing Nothing)
                ]

        it "creates component arrays" $
            getComponentArrays
                [model|
                    natural type N;
                    role type R(N);

                    system {
                        n[2] : N;
                        r[2] : R;
                        forall i : [0 .. 1]. r[i] boundto n[i];
                    }
                |]
            `shouldBe`
            Right
                [ ("n",
                    [ Component (ComponentName "n" (Just 0)) "N" Nothing Nothing
                    , Component (ComponentName "n" (Just 1)) "N" Nothing Nothing
                    ])
                , ("r",
                    [ Component
                        (ComponentName "r" (Just 0))
                        "R"
                        (Just (ComponentName "n" (Just 0)))
                        Nothing
                    , Component
                        (ComponentName "r" (Just 1))
                        "R"
                        (Just (ComponentName "n" (Just 1)))
                        Nothing
                    ])
                ]


simpleModel :: Model
simpleModel =
    [model|
        natural type N;
        role type R(N);
        compartment type C(R);

        system {
            n : N;
            r : R;
            c : C;
            r boundto n;
            r in c;
        }
    |]


checkCompartmentUpperBounds' :: Model -> Either [Error] ()
checkCompartmentUpperBounds' m =
    toEither . flip runReaderT (emptyModelInfo :&: RecursionDepth 10) $ do
        (m', info) <- typeCheck m
        local (const (info :&: RecursionDepth 10)) $ do
            (sys, _, _) <- buildSystem m'
            checkCompartmentUpperBounds sys


buildSystem' :: Model -> Either [Error] System
buildSystem' m =
    toEither . flip runReaderT (emptyModelInfo :&: RecursionDepth 10) $ do
        (m', info) <- typeCheck m
        local (const (info :&: RecursionDepth 10)) $ do
            result <- buildSystem m'
            return (view _1 result)


getComponents :: Model -> Either [Error] (Map Name Component)
getComponents m = do
    consts <- getConstants m
    return (Map.fromList (mapMaybe getComponent (Map.assocs consts)))
  where
    getComponent :: (Name, SomeExpr) -> Maybe (Name, Component)
    getComponent (name, SomeExpr (Literal comp _) (TyComponent _)) =
        Just (name, comp)
    getComponent _ = Nothing


getComponentArrays :: Model -> Either [Error] (Map Name [Component])
getComponentArrays m = do
    consts <- getConstants m
    return (Map.fromList (mapMaybe getArray (Map.assocs consts)))
  where
    getArray :: (Name, SomeExpr) -> Maybe (Name, [Component])
    getArray (name, SomeExpr (Literal arr _) (TyArray _ (TyComponent _))) =
        Just (name, toList arr)
    getArray _ = Nothing


getConstants :: Model -> Either [Error] Constants
getConstants m = toEither . flip runReaderT (emptyModelInfo :&: RecursionDepth 10) $ do
    (m', info) <- typeCheck m
    local (const (info :&: RecursionDepth 10)) $ do
        (sys, _, arrayInfos) <- buildSystem m'
        let (_, info') = updateModelInfo info arrayInfos sys
        return (view constants info')
