{-# LANGUAGE OverloadedStrings #-}


-- | Instantiation of system instances.
module Rbsc.Instantiation
    ( generateInstances
    ) where


import Control.Lens
import Control.Monad

import Data.Either
import Data.Foldable


import Rbsc.Completer

import Rbsc.Data.ModelInfo
import Rbsc.Data.System

import Rbsc.Eval

import Rbsc.Instantiation.Internal

import Rbsc.Report.Result

import Rbsc.Syntax.Typed


-- | Generate all system instances that fulfill the constraints defined in
-- the system block of the model. For each system, an extended 'ModelInfo'
-- is returned that contains constants for each component instance.
generateInstances ::
       RecursionDepth
    -> TModel
    -> ModelInfo
    -> Result' [(System, ModelInfo)]
generateInstances depth model info = do
    (sysInfos, cycles) <- fromEither' generate
    traverse_ (warn . InstantiationCycle) cycles
    return sysInfos
  where
    generate = do
        Result sys cs arrayInfos <- buildSystem depth model info

        -- We only have to check the upper role cardinality bounds, since in case
        -- the lower bounds are violated, the 'completeSystem' function will
        -- generate the missing roles.
        checkCompartmentUpperBounds (view componentTypes info) sys

        let (cycles, syss) =
                partitionEithers (completeSystem (view componentTypes info) sys)
            sysInfos = fmap (updateModelInfo info arrayInfos) syss
        sysInfos' <- filterM (checkConstraints depth cs . snd) sysInfos
        return (sysInfos', cycles)
