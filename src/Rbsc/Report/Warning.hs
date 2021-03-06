{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Rbsc.Report.Warning where


import Control.Lens

import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal


import Rbsc.Data.Action
import Rbsc.Data.Name

import Rbsc.Report
import Rbsc.Report.Region hiding (line)


data Warning
    = NonRoleInRelation !Region
    | NonCompartmentInRelation !Region
    | InstantiationCycle [TypeName]
    | OutOfRangeUpdate !Region (Int, Int) !Int
    | InconsistentActionIndices !Region !Int !Region !Int
    | UnsynchronizedAction !(Loc Action)
    | MissingOverriddenAction !(Loc Action) !ComponentName
    | ConvertedToArray !Region !Int
    | DynamicArrayAccess !Region
    | MergedComponent !Region !Region
    deriving (Eq, Show)

makePrisms ''Warning


toReport :: Warning -> Report
toReport = \case
    NonRoleInRelation rgn ->
        warningReport "expression is always false"
            [ hintPart rgn . Just $
                "this expression is always false" <> line <>
                "(the left-hand side does not have a role type)"
            ]

    NonCompartmentInRelation rgn ->
        warningReport "expression is always false"
            [ hintPart rgn . Just $
                "this expression is always false" <> line <>
                "(the right-hand side does not have a compartment type)"
            ]

    InstantiationCycle tyNames ->
        flip warningReport [] $
            "omitted system instance because of cycle among types:\n" <>
            Text.intercalate ", " (fmap getTypeName tyNames)

    OutOfRangeUpdate rgn (lower, upper) actual ->
        warningReport "out-of-range update"
            [ hintPart rgn . Just $
                "expression evaluates to" <+> pretty actual <>
                ", but variable has range [" <> pretty lower <+> ".." <+>
                pretty upper <> "]"
            ]

    InconsistentActionIndices rgn1 n1 rgn2 n2 ->
        let ((rgn1', n1'), (rgn2', n2')) = if rgn1 < rgn2
                then ((rgn1, n1), (rgn2, n2))
                else ((rgn2, n2), (rgn1, n1))
        in warningReport "inconsistent action indices"
            [ hintPart rgn1' . Just $
                "the action has" <+> indexText n1' <+> "here ..."
            , hintPart rgn2' . Just $
                "... but" <+> indexText n2' <+> " here"
            ]

    UnsynchronizedAction act ->
        warningReport "unsynchronized action"
            [ hintPart (getLoc act) . Just $
                "the action" <+> annotate bold (pretty act) <+>
                "does not synchronize with any other action"
            ]

    MissingOverriddenAction act compName ->
        warningReport "missing overridden action"
            [ hintPart (getLoc act) . Just $
                "the action" <+> annotate bold (pretty act) <+>
                "is marked override," <> line <>
                "but the player" <+> annotate bold (pretty compName) <+>
                "does not provide a matching action"
            ]

    DynamicArrayAccess rgn ->
        warningReport "dynamic array access"
            [ hintPart rgn . Just $
                "this index depends on one or more variables" <> line <>
                "the bounds are not checked"
            ]

    ConvertedToArray rgn size ->
        warningReport "converted value to array"
            [ hintPart rgn . Just $
                "value has been converted to an array of size" <+> pretty size
            ]

    MergedComponent rgn first ->
        warningReport "merged components"
            [ hintPart rgn . Just $
                "compontent has been merged with component of the same name"
            , hintPart first . Just $
                "first definition was here"
            ]


warningReport :: Text -> [Part] -> Report
warningReport title = hintReport ("warning: " <> title)


indexText :: Int -> Doc ann
indexText 1 = "1 index"
indexText n = pretty n <+> "indices"
