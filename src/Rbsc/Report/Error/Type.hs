{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Representation of type errors.
module Rbsc.Report.Error.Type
    ( Error(..)
    , toReport

    , _TypeError
    , _NotComparable
    , _UndefinedType
    , _UndefinedIdentifier
    ) where


import Control.Lens

import           Data.Monoid ((<>))
import           Data.Text   (Text)
import qualified Data.Text   as Text

import Rbsc.Report
import Rbsc.Report.Region


-- | Represents a type error.
data Error
    = TypeError [Text] !Text !Region
    | NotComparable !Text !Region
    | NotAnArray !Text !Region
    | UndefinedType !Region
    | UndefinedIdentifier !Region
    deriving (Eq, Show)


toReport :: Error -> Report
toReport = \case
    TypeError expected actual rgn ->
        Report "type error"
            [ errorPart rgn . Just $
                "expression has type: " <> actual <>
                "\nexpected type: " <> orList expected
            ]

    NotAnArray actual rgn ->
        Report "not an array"
            [ errorPart rgn . Just $
                "expression has type: " <> actual <>
                "\nexpected type: array"
            ]

    NotComparable ty rgn ->
        Report "uncomparable values"
            [ errorPart rgn . Just $
                "values of type " <> ty <> " are not comparable"
            ]

    UndefinedType rgn ->
        Report "undefined type" [errorPart rgn Nothing]

    UndefinedIdentifier rgn ->
        Report "undefined identifier" [errorPart rgn Nothing]


orList :: [Text] -> Text
orList []     = Text.empty
orList [x]    = x
orList [x, y] = x <> " or " <> y
orList (x:xs) = x <> ", " <> orList xs


makePrisms ''Error
