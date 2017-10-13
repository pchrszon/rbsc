{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


-- | Syntax-related errors.
module Rbsc.Report.Error.Syntax
    ( Error(..)
    , toReport
    ) where


import Rbsc.Report
import Rbsc.Report.Region


-- | Represents a syntax-related error.
data Error
    = UndefinedType !Region
    | DuplicateType !Region !Region
    | NonRoleInCompartment !Region
    deriving (Show)


toReport :: Error -> Report
toReport = \case
    UndefinedType rgn ->
        Report "undefined type" [Part rgn Nothing]

    DuplicateType second first ->
        Report "duplicate type definition"
            [ Part second (Just "a type of the same name was defined already")
            , Part first (Just "first definition was here")
            ]

    NonRoleInCompartment rgn ->
        Report "only roles can be contained in compartments"
            [ Part rgn (Just "this is not a role type")
            ]
