{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Representation of type errors.
module Rbsc.Report.Error.Type
    ( Error(..)
    , toReport

    , _TypeError
    ) where


import Control.Lens

import Data.Monoid ((<>))
import Data.Text   (Text)

import Rbsc.Report
import Rbsc.Report.Region


-- | Represents a type error.
data Error
    = TypeError !Text !Text !Region
    | UndefinedType !Region
    | UndefinedIdentifier !Region
    deriving (Eq, Show)


toReport :: Error -> Report
toReport = \case
    TypeError expected actual rgn ->
        Report "type error"
            [ errorPart rgn . Just $
                "unexpected type: " <> actual <> "\nexpected type: " <> expected
            ]

    UndefinedType rgn ->
        Report "undefined type" [errorPart rgn Nothing]

    UndefinedIdentifier rgn ->
        Report "undefined identifier" [errorPart rgn Nothing]


makePrisms ''Error
