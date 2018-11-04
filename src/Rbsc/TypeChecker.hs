{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase #-}


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

import Rbsc.TypeChecker.Coordinator
import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Impl
import Rbsc.TypeChecker.Internal
import Rbsc.TypeChecker.ModelInfo


typeCheck
    :: (MonadReader r (t Result), HasRecursionDepth r, MonadTrans t)
    => U.Model
    -> t Result (T.Model, ModelInfo)
typeCheck model = do
    (info, consts') <- getModelInfo model
    let compTys  = view MI.componentTypes info
        symTable = view MI.symbolTable info
        consts   = view MI.constants info
    depth  <- view recursionDepth

    model' <- lift
        (runTypeChecker (tcModel model consts') compTys symTable consts depth)
    return (model', info)


tcModel :: U.Model -> [TConstant] -> TypeChecker T.Model
tcModel U.Model{..} consts = T.Model consts
    <$> traverse tcVarDecl modelGlobals
    <*> traverse tcLabel modelLabels
    <*> traverse tcConstraint modelSystem
    <*> tcImpls modelImpls
    <*> traverse tcCoordinator modelCoordinators
    <*> traverse tcRewardStruct modelRewardStructs


tcLabel :: ULabel -> TypeChecker TLabel
tcLabel Label{..} = do
    e' <- labelExpr `hasType` TyBool
    return (Label labelName (SomeExpr e' TyBool `withLocOf` labelExpr))


tcConstraint :: LExpr -> TypeChecker (Loc (T.Expr Bool))
tcConstraint e = do
    e' <- e `hasType` TyBool
    return (e' `withLocOf` e)


tcRewardStruct :: URewardStruct -> TypeChecker (TRewardStruct ElemMulti)
tcRewardStruct RewardStruct{..} = RewardStruct rsName
    <$> tcElemMultis tcRewardStructItem rsItems


tcRewardStructItem :: URewardStructItem -> TypeChecker TRewardStructItem
tcRewardStructItem RewardStructItem{..} = RewardStructItem
    <$> tcRewardKind riKind
    <*> someExpr riGuard TyBool
    <*> someExpr riReward TyDouble


tcRewardKind :: URewardKind -> TypeChecker TRewardKind
tcRewardKind = \case
    TransitionReward mAct mConstr -> TransitionReward
        <$> traverse (\act -> (`withLocOf` act) <$> tcAction act) mAct
        <*> traverse tcPlayingConstraint mConstr
    StateReward -> return StateReward
