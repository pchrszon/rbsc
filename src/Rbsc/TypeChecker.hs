{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Type checker for 'Model's.
module Rbsc.TypeChecker
    ( typeCheck
    ) where


import Control.Lens
import Control.Monad.Reader


import Rbsc.Config

import           Rbsc.Data.ModelInfo (ModelInfo)
import qualified Rbsc.Data.ModelInfo as MI
import           Rbsc.Data.Type

import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed   hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Untyped as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Impl
import Rbsc.TypeChecker.Internal
import Rbsc.TypeChecker.ModelInfo


typeCheck ::
       (MonadReader r (t (Result Errors)), HasRecursionDepth r, MonadTrans t)
    => U.Model
    -> t (Result Errors) (T.Model, ModelInfo)
typeCheck model = do
    (info, consts') <- getModelInfo model
    let compTys  = view MI.componentTypes info
        symTable = view MI.symbolTable info

    model' <- lift (runTypeChecker (tcModel model consts') compTys symTable)
    return (model', info)


tcModel :: U.Model -> [TConstant] -> TypeChecker T.Model
tcModel U.Model{..} consts = T.Model consts
    <$> traverse tcVarDecl modelGlobals
    <*> traverse tcConstraint modelSystem
    <*> tcImpls modelImpls


tcConstraint :: LExpr -> TypeChecker (Loc (T.Expr Bool))
tcConstraint e = do
    e' <- e `hasType` TyBool
    return (e' `withLocOf` e)
