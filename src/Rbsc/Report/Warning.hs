{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Rbsc.Report.Warning where


import Data.Monoid
import qualified Data.Text as Text


import Rbsc.Data.Name

import Rbsc.Report


data Warning
    = InstantiationCycle [TypeName]
    deriving (Eq, Show)


toReport :: Warning -> Report
toReport = addWarningLabel . \case
    InstantiationCycle tyNames ->
        flip Report [] $
            "omitted system instance because of cycle among types:\n" <>
            Text.intercalate ", " (fmap getTypeName tyNames)


addWarningLabel :: Report -> Report
addWarningLabel (Report t ps) = Report ("warning: " <> t) ps
