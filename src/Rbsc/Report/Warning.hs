{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Report.Warning where


import           Data.Text (Text, pack)
import qualified Data.Text as Text


import Rbsc.Data.Name

import Rbsc.Report
import Rbsc.Report.Region


data Warning
    = NonRoleInRelation !Region
    | NonCompartmentInRelation !Region
    | InstantiationCycle [TypeName]
    | OutOfRangeUpdate !Region (Int, Int) !Int
    deriving (Eq, Show)


toReport :: Warning -> Report
toReport = \case
    NonRoleInRelation rgn ->
        warningReport "expression is always false"
            [ hintPart rgn . Just $
                "this expression is always false\n" <>
                "(the left-hand side does not have a role type)"
            ]

    NonCompartmentInRelation rgn ->
        warningReport "expression is always false"
            [ hintPart rgn . Just $
                "this expression is always false\n" <>
                "(the right-hand side does not have a compartment type)"
            ]

    InstantiationCycle tyNames ->
        flip warningReport [] $
            "omitted system instance because of cycle among types:\n" <>
            Text.intercalate ", " (fmap getTypeName tyNames)

    OutOfRangeUpdate rgn (lower, upper) actual ->
        warningReport "out-of-range update"
            [ hintPart rgn . Just $
                "expression evaluates to " <> pack (show actual) <>
                ", but variable has range [" <> pack (show lower) <> ".." <>
                pack (show upper) <> "]"
            ]


warningReport :: Text -> [Part] -> Report
warningReport title = hintReport ("warning: " <> title)
