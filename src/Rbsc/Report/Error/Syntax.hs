{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Syntax-related errors.
module Rbsc.Report.Error.Syntax
    ( Error(..)
    , toReport

    , _ParseError
    , _UndefinedType
    , _DuplicateType
    , _NonRoleInCompartment
    , _UndefinedIdentifier
    , _DuplicateIdentifier
    , _CyclicDefinition
    ) where


import Control.Lens

import Data.Monoid
import Data.Text   (Text)


import Rbsc.Report
import Rbsc.Report.Region


-- | Represents a syntax-related error.
data Error
    = ParseError !Region !Text
    | UndefinedType !Region
    | DuplicateType !Region !Region
    | NonRoleInCompartment !Region
    | UndefinedIdentifier !Region
    | DuplicateIdentifier !Region !Region
    | CyclicDefinition !Text !Region [Region]
    deriving (Eq, Show)


toReport :: Error -> Report
toReport = \case
    ParseError rgn desc ->
        Report "syntax error" [errorPart rgn (Just desc)]

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

    UndefinedIdentifier rgn ->
        Report "undefined identifier" [errorPart rgn Nothing]

    DuplicateIdentifier second first ->
        Report "duplicate definition"
            [ errorPart second
                (Just "an identifier with this name already exists")
            , hintPart first (Just "first definition was here")
            ]

    CyclicDefinition construct rgn rgns ->
        Report "cyclic definition" $
            (errorPart rgn (Just $
                "this " <> construct <> " is defined in terms of itself")) :
            fmap (\r -> hintPart r Nothing) rgns

makePrisms ''Error
