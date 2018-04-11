{-# LANGUAGE OverloadedStrings #-}


-- | Instantiation of system instances.
module Rbsc.Instantiation
    ( generateInstances
    ) where


import Control.Lens
import Control.Monad

import Data.Either


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
generateInstances depth model info = fromEither' $ do
    Result sys cs arrayInfos <- buildSystem depth model info

    -- We only have to check the upper role cardinality bounds, since in case
    -- the lower bounds are violated, the 'completeSystem' function will
    -- generate the missing roles.
    checkCompartmentUpperBounds (view componentTypes info) sys

    let syss = rights (completeSystem (view componentTypes info) sys) -- TODO: cycle warnings
        sysInfos = fmap (updateModelInfo info arrayInfos) syss
    filterM (checkConstraints depth cs . snd) sysInfos
