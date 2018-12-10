{-# LANGUAGE FlexibleContexts      #-}
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
import Data.List.NonEmpty (NonEmpty, nonEmpty)


import Rbsc.Completer

import Rbsc.Config

import Rbsc.Data.Field
import Rbsc.Data.ModelInfo
import Rbsc.Data.System

import Rbsc.Instantiation.Internal

import Rbsc.Report.Error
import Rbsc.Report.Result

import Rbsc.Syntax.Typed


-- | Generate all system instances that fulfill the constraints defined in
-- the system block of the model. For each system, an extended 'ModelInfo'
-- is returned that contains constants for each component instance.
generateInstances ::
       (MonadReader r (t Result), Has RecursionDepth r, MonadTrans t)
    => Model
    -> ModelInfo
    -> t Result (NonEmpty (System, ModelInfo))
generateInstances model info = do
    depth <- view recursionDepth
    lift (runReaderT generate (info :&: depth))
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

        depth <- view recursionDepth
        sysInfos' <- flip filterM sysInfos $ \(_, info') ->
            local (const (info' :&: depth)) (checkConstraints constraints)

        case nonEmpty sysInfos' of
            Just sysInfos'' -> do
                for_ sysInfos'' $ \(sys', _) ->
                    when (null (view instances sys')) (throwNoLoc EmptySystem)
                return sysInfos''
            Nothing -> throwNoLoc NoSystems
