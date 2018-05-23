{-# LANGUAGE MultiParamTypeClasses #-}


-- | Instantiation of system instances.
module Rbsc.Instantiation
    ( generateInstances
    ) where


import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.Either
import Data.Foldable


import Rbsc.Completer

import Rbsc.Config

import Rbsc.Data.Info
import Rbsc.Data.ModelInfo
import Rbsc.Data.System

import Rbsc.Instantiation.Internal

import Rbsc.Report.Result

import Rbsc.Syntax.Typed


-- | Generate all system instances that fulfill the constraints defined in
-- the system block of the model. For each system, an extended 'ModelInfo'
-- is returned that contains constants for each component instance.
generateInstances ::
       (MonadReader r (t Result), HasRecursionDepth r, MonadTrans t)
    => Model
    -> ModelInfo
    -> t Result [(System, ModelInfo)]
generateInstances model info = do
    depth <- view recursionDepth
    lift (runReaderT generate (Info info depth))
  where
    generate = do
        -- Partition the system block of the model into a 'System' and
        -- a list of other constraints.
        (sys, constraints, arrayInfos) <- buildSystem model

        -- We only have to check the upper role cardinality bounds, since in case
        -- the lower bounds are violated, the 'completeSystem' function will
        -- generate the missing roles.
        checkCompartmentUpperBounds sys

        (cycles, syss) <- partitionEithers <$> completeSystem sys
        lift (traverse_ (warn . InstantiationCycle) cycles)

        let sysInfos = fmap (updateModelInfo info arrayInfos) syss

        flip filterM sysInfos $ \(_, info') ->
            local (set modelInfo info') (checkConstraints constraints)
