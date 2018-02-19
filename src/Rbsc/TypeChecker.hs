{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Type checker for 'Model's.
module Rbsc.TypeChecker
    ( SomeExpr(..)
    , getExpr

    , typeCheck
    , extract
    ) where


import Rbsc.Data.ComponentType
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region (withLocOf)

import Rbsc.Syntax.Expr.Typed (SomeExpr (..))
import Rbsc.Syntax.Typed      hiding (Type (..))
import Rbsc.Syntax.Untyped    hiding (Type (..))

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal


-- | Type check a 'Model'. All untyped expressions in the model are type
-- checked and replaced by 'SomeExpr'.
typeCheck ::
       ComponentTypes
    -> SymbolTable
    -> UModel
    -> [TConstant]
    -> [TFunction]
    -> Either Error TModel
typeCheck types symTable m consts funcs =
    runTypeChecker (tcModel m consts funcs) types symTable


tcModel :: UModel -> [TConstant] -> [TFunction] -> TypeChecker TModel
tcModel Model{..} consts funcs =
    Model consts funcs modelNaturalTypes modelRoleTypes modelCompartmentTypes
    <$> traverse tcConstraint modelSystem


tcConstraint :: LExpr -> TypeChecker LSomeExpr
tcConstraint e = do
    e' <- e `hasType` TyBool
    return (SomeExpr e' TyBool `withLocOf` e)
