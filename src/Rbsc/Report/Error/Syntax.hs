{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Syntax-related errors.
module Rbsc.Report.Error.Syntax
    ( Error(..)
    , toReport
    ) where


import Data.Text (Text)

import Rbsc.Report
import Rbsc.Report.Region


-- | Represents a syntax-related error.
data Error
    = ParseError !Region !Text
    | UndefinedType !Region
    | DuplicateType !Region !Region
    | NonRoleInCompartment !Region
    | DuplicateIdentifier !Region !Region
    deriving (Show)


toReport :: Error -> Report
toReport = \case
    ParseError rgn desc ->
        Report "parse error" [errorPart rgn (Just desc)]

    UndefinedType rgn ->
        Report "undefined type" [errorPart rgn Nothing]

    DuplicateType second first ->
        Report "duplicate type definition"
            [ errorPart second (Just "a type of the same name already exists")
            , hintPart first (Just "first definition was here")
            ]

    NonRoleInCompartment rgn ->
        Report "only roles can be contained in compartments"
            [ errorPart rgn (Just "this is not a role type")
            ]

    DuplicateIdentifier second first ->
        Report "duplicate definition"
            [ errorPart second
                (Just "an identifier with this name already exists")
            , hintPart first (Just "first definition was here")
            ]
