{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Rbsc.EvalSpec (spec) where


import Control.Lens

import qualified Data.Map.Strict as Map

import Test.Hspec

import qualified Rbsc.Syntax.Expr.Typed   as T
import qualified Rbsc.Syntax.Expr.Untyped as U
import           Rbsc.Syntax.Operators

import qualified Rbsc.Report.Error.Eval as Eval
import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region     (Loc (..))


import Rbsc.Eval
import Rbsc.TypeChecker

import Rbsc.Data.SymbolTable
import Rbsc.Data.Type
import Rbsc.Data.Value

import Rbsc.Parser.TH


spec :: Spec
spec = do
    describe "eval" $ do
        it "evaluates constant expressions" $
            eval' TyInt [expr| (x + 1) * 2 |]
            `shouldBe`
            Right 4

        it "detects division by zero" $
            eval' TyDouble [expr| 1.0 / 0 |]
            `shouldSatisfy`
            has (_Left._Right.Eval._DivisionByZero)

    describe "reduce" $ do
        it "evaluates constant expressions" $
            reduce' TyBool [expr| (x + 1) * 2 = 4 |]
            `shouldBe`
            Right (T.Literal True)

        it "evaluates constant sub-expressions" $
            reduce' TyBool [expr| y + 1 < x * 2 |]
            `shouldBe`
            Right
                (T.RelOp
                     TyInt
                     Lt
                     (T.ArithOp Add (T.Variable "y" TyInt) (T.Literal 1))
                     (T.Literal 2))


constants :: Constants
constants = Map.fromList
    [ ("x", Value 1 TyInt)
    ]


symbolTable :: SymbolTable
symbolTable = Map.fromList
    [ ("x", AType TyInt)
    , ("y", AType TyInt)
    ]


eval' :: Type t -> Loc U.Expr -> Either (Either Type.Error Eval.Error) t
eval' ty e = case typeCheck Map.empty symbolTable e >>= extract ty (getLoc e) of
    Left err -> Left (Left err)
    Right e' -> case eval constants (Loc e' (getLoc e)) of
        Left err -> Left (Right err)
        Right x  -> Right x


reduce' ::
       Type t -> Loc U.Expr -> Either (Either Type.Error Eval.Error) (T.Expr t)
reduce' ty e =
    case typeCheck Map.empty symbolTable e >>= extract ty (getLoc e) of
        Left err -> Left (Left err)
        Right e' -> case reduce constants e' of
            Left err  -> Left (Right err)
            Right e'' -> Right e''
