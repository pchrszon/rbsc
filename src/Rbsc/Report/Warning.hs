{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Report.Warning where


import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as Text


import Rbsc.Data.Name

import Rbsc.Report


data Warning
    = InstantiationCycle [TypeName]
    deriving (Eq, Show)


toReport :: Warning -> Report
toReport = \case
    InstantiationCycle tyNames ->
        flip warningReport [] $
            "omitted system instance because of cycle among types:\n" <>
            Text.intercalate ", " (fmap getTypeName tyNames)


warningReport :: Text -> [Part] -> Report
warningReport title = hintReport ("warning: " <> title)
