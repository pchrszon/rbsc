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
import Rbsc.Data.SymbolTable
import Rbsc.Data.Type

import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region     (withLocOf)

import Rbsc.Syntax.Expr.Typed (SomeExpr (..))
import Rbsc.Syntax.Typed      hiding (Type (..))
import Rbsc.Syntax.Untyped    hiding (Type (..))

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal


-- | Type check a 'Model'. All untyped expressions in the model are type
-- checked and replaced by 'SomeExpr'.
typeCheck :: ComponentTypes -> SymbolTable -> UModel -> Either Type.Error TModel
typeCheck types symTable m = runTypeChecker (tcModel m) types symTable


tcModel :: UModel -> TypeChecker TModel
tcModel Model{..} = Model
    <$> traverse tcConstant constants
    <*> pure naturalTypes
    <*> pure roleTypes
    <*> pure compartmentTypes
    <*> traverse tcConstraint system


tcConstant :: UConstant -> TypeChecker TConstant
tcConstant (Constant name sTy e) = case fromSyntaxType sTy of
    SomeType ty -> do
        e' <- e `hasType` ty
        return (Constant name sTy (SomeExpr e' ty `withLocOf` e))


tcConstraint :: LExpr -> TypeChecker LSomeExpr
tcConstraint e = do
    e' <- e `hasType` TyBool
    return (SomeExpr e' TyBool `withLocOf` e)
