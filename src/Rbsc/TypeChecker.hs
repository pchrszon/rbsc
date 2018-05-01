{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Type checker for 'Model's.
module Rbsc.TypeChecker
    ( typeCheck
    ) where

import Control.Lens

import           Rbsc.Data.ModelInfo (ModelInfo)
import qualified Rbsc.Data.ModelInfo as MI
import           Rbsc.Data.Type
import           Rbsc.Data.Scope

import Rbsc.Eval

import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed   hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Untyped as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal
import Rbsc.TypeChecker.ModelInfo


typeCheck :: RecursionDepth -> U.Model -> Result' (T.Model, ModelInfo)
typeCheck depth model = do
    (info, consts') <- getModelInfo depth model
    let compTys  = view MI.componentTypes info
        symTable = view MI.symbolTable info

    model' <- runTypeChecker (tcModel model consts') compTys symTable
    return (model', info)


tcModel :: U.Model -> [TConstant] -> TypeChecker T.Model
tcModel U.Model{..} consts = T.Model consts
    <$> traverse tcGlobal modelGlobals
    <*> traverse tcConstraint modelSystem


tcGlobal :: UGlobal -> TypeChecker (Name, Maybe LSomeExpr)
tcGlobal (Global (VarDecl (Loc name _) _ mInit)) =
    view (symbolTable.at (ScopedName GlobalScope name)) >>= \case
        Just (SomeType ty) -> case mInit of
            Just e -> do
                e' <- e `hasType` ty
                return (name, Just (SomeExpr e' ty `withLocOf` e))
            Nothing -> return (name, Nothing)
        Nothing -> error ("tcGlobal: " ++ show name ++ " not in symbol table")


tcConstraint :: LExpr -> TypeChecker (Loc (T.Expr Bool))
tcConstraint e = do
    e' <- e `hasType` TyBool
    return (e' `withLocOf` e)
