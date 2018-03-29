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

import Rbsc.Report.Error

import Rbsc.Syntax.Typed


-- | Generate all system instances that fulfill the constraints defined in
-- the system block of the model. For each system, an extended 'ModelInfo'
-- is returned that contains constants for each component instance.
generateInstances ::
       RecursionDepth
    -> TModel
    -> ModelInfo
    -> Either Error [(System, ModelInfo)]
generateInstances depth model info = do
    BuilderState sys cs arrayInfos <- buildSystem depth model info
    let syss = rights (completeSystem (view componentTypes info) sys) -- TODO: cycle warnings
        sysInfos = fmap (updateModelInfo info arrayInfos) syss
    filterM (checkConstraints depth cs . snd) sysInfos
