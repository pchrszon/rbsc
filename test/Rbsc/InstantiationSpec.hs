{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.InstantiationSpec (spec) where


import Control.Lens

import Data.Foldable
import Data.Maybe

import Test.Hspec

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Rbsc.Data.Component
import Rbsc.Data.ModelInfo
import Rbsc.Data.Name
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Instantiation.Internal

import Rbsc.Parser.TH

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Expr.Typed (Expr (..), SomeExpr (..))
import Rbsc.Syntax.Untyped    (Model)

import Rbsc.TypeChecker


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
                    system
                        { n[2]: N
                        , r: R
                        , r boundto n[1]
                        }
                |]
            `shouldBe`
            Right System
                { _instances =
                    [ ("n[0]", "N")
                    , ("n[1]", "N")
                    , ("r", "R")
                    ]
                , _boundTo = [("r", "n[1]")]
                , _containedIn = []
                }

        it "handles quantification" $
            buildSystem'
                [model|
                    natural type N;
                    role type R(N);

                    const SIZE = 2;

                    system
                        { n[SIZE]: N
                        , r[SIZE]: R
                        , forall i: [0 .. SIZE - 1]. r[i] boundto n[i]
                        }
                |]
            `shouldBe`
            Right System
                { _instances =
                    [ ("n[0]", "N")
                    , ("n[1]", "N")
                    , ("r[0]", "R")
                    , ("r[1]", "R")
                    ]
                , _boundTo =
                    [ ("r[0]", "n[0]")
                    , ("r[1]", "n[1]")
                    ]
                , _containedIn = []
                }

        it "detects invalid use of 'boundto' relation" $
            buildSystem'
                [model|
                    natural type N;
                    natural type M;
                    system
                        { n: N
                        , m: M
                        , n boundto m
                        }
                |]
            `shouldSatisfy`
            has (_Left.traverse.errorDesc._NotARole)

        it "detects invalid bindings" $
            buildSystem'
                [model|
                    natural type N;
                    natural type M;
                    role type R(N);
                    system
                        { m: M
                        , r: R
                        , r boundto m
                        }
                |]
            `shouldSatisfy`
            has (_Left.traverse.errorDesc._InvalidBinding)

        it "detects invalid use of 'in' relation" $
            buildSystem'
                [model|
                    natural type N;
                    role type R(N);
                    system
                        { n: N
                        , r: R
                        , r in n
                        }
                |]
            `shouldSatisfy`
            has (_Left.traverse.errorDesc._NotACompartment)

        it "detects invalid component array sizes" $
            buildSystem'
                [model|
                    natural type N;
                    system { n[-1]: N }
                |]
            `shouldSatisfy`
            has (_Left.traverse.errorDesc._InvalidUpperBound)


    describe "checkCompartmentUpperBounds" $
        it "detects overfull compartments" $
            checkCompartmentUpperBounds'
                [model|
                    natural type N;
                    role type R(N);
                    compartment type C(R);

                    system
                        { r[2]: R
                        , c: C
                        , r[0] in c
                        , r[1] in c
                        }
                |]
            `shouldBe`
            Left [Error dummyRegion (TooManyRoles "c" [("R", 1)])]


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

                    system
                        { n[2]: N
                        , r[2]: R
                        , forall i: [0 .. 1]. r[i] boundto n[i]
                        }
                |]
            `shouldBe`
            Right
                [ ("n", [ Component "n[0]" "N" Nothing Nothing
                        , Component "n[1]" "N" Nothing Nothing
                        ])
                , ("r", [ Component "r[0]" "R" (Just "n[0]") Nothing
                        , Component "r[1]" "R" (Just "n[1]") Nothing
                        ])
                ]


simpleModel :: Model
simpleModel =
    [model|
        natural type N;
        role type R(N);
        compartment type C(R);

        system
            { n: N
            , r: R
            , c: C
            , r boundto n
            , r in c
            }
    |]


checkCompartmentUpperBounds' :: Model -> Either [Error] ()
checkCompartmentUpperBounds' m = do
    (m', info) <- toEither (typeCheck 10 m)
    Result sys _ _ <- over _Left (: []) (buildSystem 10 m' info)
    over _Left (: [])
        (checkCompartmentUpperBounds (view componentTypes info) sys)


dummyRegion :: Region
dummyRegion = Region "" "" (Position 1 1) (Position 1 2)


buildSystem' :: Model -> Either [Error] System
buildSystem' m = do
    (m', info) <- toEither (typeCheck 10 m)
    result <- over _Left (: []) (buildSystem 10 m' info)
    return (_system result)


getComponents :: Model -> Either [Error] (Map Name Component)
getComponents m = do
    consts <- getConstants m
    return (Map.fromList (mapMaybe getComponent (Map.assocs consts)))
  where
    getComponent :: (Name, SomeExpr) -> Maybe (Name, Component)
    getComponent (name, SomeExpr (Literal comp) (TyComponent _)) =
        Just (name, comp)
    getComponent _ = Nothing


getComponentArrays :: Model -> Either [Error] (Map Name [Component])
getComponentArrays m = do
    consts <- getConstants m
    return (Map.fromList (mapMaybe getArray (Map.assocs consts)))
  where
    getArray :: (Name, SomeExpr) -> Maybe (Name, [Component])
    getArray (name, SomeExpr (Literal arr) (TyArray _ (TyComponent _))) =
        Just (name, toList arr)
    getArray _ = Nothing


getConstants :: Model -> Either [Error] Constants
getConstants m = do
    (m', info) <- toEither (typeCheck 10 m)
    Result sys _ arrayInfos <- over _Left (: []) (buildSystem 10 m' info)
    let (_, info') = updateModelInfo info arrayInfos sys
    return (view constants info')
