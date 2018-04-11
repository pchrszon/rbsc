{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Type checker for 'Model's.
module Rbsc.TypeChecker
    ( typeCheck
    ) where

import Control.Lens

import Rbsc.Data.ModelInfo
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region (withLocOf)
import Rbsc.Report.Result

import Rbsc.Syntax.Expr.Typed (SomeExpr (..))
import Rbsc.Syntax.Typed      hiding (Type (..))
import Rbsc.Syntax.Untyped    hiding (Type (..))

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal  (TypeChecker, runTypeChecker)
import Rbsc.TypeChecker.ModelInfo


typeCheck :: RecursionDepth -> UModel -> Result' (TModel, ModelInfo)
typeCheck depth model = fromEither $ do
    (info, consts') <- getModelInfo depth model
    let compTys  = view componentTypes info
        symTable = view symbolTable info

    model' <- over _Left (: []) $
        runTypeChecker (tcModel model consts') compTys symTable

    return (model', info)


tcModel :: UModel -> [TConstant] -> TypeChecker TModel
tcModel Model{..} consts =
    Model consts [] [] [] []
    <$> traverse tcConstraint modelSystem


tcConstraint :: LExpr -> TypeChecker LSomeExpr
tcConstraint e = do
    e' <- e `hasType` TyBool
    return (SomeExpr e' TyBool `withLocOf` e)
