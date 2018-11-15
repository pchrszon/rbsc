{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


-- | Type checker for 'Model's.
module Rbsc.TypeChecker
    ( typeCheck
    ) where


import Control.Lens
import Control.Monad.Reader

import Data.Map.Strict (Map)


import Rbsc.Config

import Rbsc.Data.ModelInfo (ModelInfo)
import Rbsc.Data.Type

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
    (info, insts) <- getModelInfo model
    depth         <- view recursionDepth
    model'        <- lift (runTypeChecker (tcModel insts model) info depth)
    return (model', info)


tcModel :: Map TypeName [UModuleInstance] -> U.Model -> TypeChecker T.Model
tcModel insts U.Model{..} = T.Model
    <$> traverse tcVarDecl modelGlobals
    <*> traverse tcLabel modelLabels
    <*> traverse tcConstraint modelSystem
    <*> tcModuleInstances insts
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
