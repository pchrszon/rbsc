{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Representation of model errors.
module Rbsc.Report.Error where


import Control.Lens
import Control.Monad.Except

import Data.Monoid

import           Data.Text (Text, pack)
import qualified Data.Text as Text


import Rbsc.Data.Name

import Rbsc.Report
import Rbsc.Report.Region


data Error = Error
    { _errorRegion :: !Region
    , _errorDesc   :: !ErrorDesc
    } deriving (Eq, Show)


data ErrorDesc
    = ParseError !Text
    | UndefinedType
    | DuplicateType !Region
    | NonRoleInCompartment
    | UndefinedIdentifier
    | DuplicateIdentifier !Region
    | CyclicDefinition !Text [Region]

    | TypeError [Text] !Text
    | NotComparable !Text
    | NotAnArray !Text
    | NotAFunction !Text
    | WrongNumberOfArguments !Int !Int
    | NotARole
    | NotACompartment
    | InvalidBinding !TypeName !TypeName
    | InvalidLowerBound !Int
    | InvalidCardinalities !Int !Int
    | TooManyRoles !Name [(TypeName, Int)]

    | DivisionByZero
    | NotConstant
    | InvalidUpperBound !Int
    | IndexOutOfBounds (Int, Int) !Int
    | ExceededDepth
    deriving (Eq, Show)


makeLenses ''Error
makePrisms ''ErrorDesc


toReport :: Error -> Report
toReport (Error rgn desc) = case desc of
    ParseError err ->
        errorReport "syntax error" [errorPart rgn (Just err)]

    UndefinedType ->
        errorReport "undefined type" [errorPart rgn Nothing]

    DuplicateType first ->
        errorReport "duplicate type definition"
            [ errorPart rgn (Just "a type of the same name already exists")
            , hintPart first (Just "first definition was here")
            ]

    NonRoleInCompartment ->
        errorReport "only roles can be contained in compartments"
            [ errorPart rgn (Just "this is not a role type")
            ]

    UndefinedIdentifier ->
        errorReport "undefined identifier" [errorPart rgn Nothing]

    DuplicateIdentifier first ->
        errorReport "duplicate definition"
            [ errorPart rgn
                (Just "an identifier with this name already exists")
            , hintPart first (Just "first definition was here")
            ]

    CyclicDefinition construct rgns ->
        errorReport "cyclic definition" $
            errorPart rgn (Just $
                "this " <> construct <> " is defined in terms of itself") :
            fmap (`hintPart` Nothing) rgns

    TypeError expected actual ->
        errorReport "type error"
            [ errorPart rgn . Just $
                "expression has type: " <> actual <>
                "\nexpected type: " <> list "or" expected
            ]

    NotComparable ty ->
        errorReport "uncomparable values"
            [ errorPart rgn . Just $
                "values of type " <> ty <> " are not comparable"
            ]

    NotAnArray actual ->
        errorReport "not an array"
            [ errorPart rgn . Just $
                "expression has type: " <> actual <>
                "\nexpected type: array"
            ]

    NotAFunction ty ->
        errorReport "not a function"
            [ errorPart rgn . Just $
                "this is not a function\nexpression has type: " <> ty
            ]

    WrongNumberOfArguments expected actual ->
        errorReport "wrong number of arguments"
            [ errorPart rgn . Just $
                "arguments given: " <> Text.pack (show actual) <>
                "\nexpected: " <> Text.pack (show expected)
            ]

    NotARole ->
        errorReport "component is not a role"
            [ errorPart rgn . Just $
                "only roles can be bound to players and\n" <>
                "be contained in compartments"
            ]

    NotACompartment ->
        errorReport "component is not a compartment"
            [ errorPart rgn (Just "only compartments can contain roles")
            ]

    InvalidBinding roleTyName playerTyName ->
        errorReport "invalid binding"
            [ errorPart rgn . Just $
                "a role of type " <> getTypeName roleTyName <>
                " cannot be bound to a player of type " <>
                getTypeName playerTyName
            ]

    InvalidLowerBound lower ->
        errorReport "invalid cardinalities"
            [ errorPart rgn . Just $
                "lower bound must be greater or equal 0, " <>
                "but the given bound is " <> pack (show lower)
            ]

    InvalidCardinalities lower upper ->
        errorReport "invalid cardinalities"
            [ errorPart rgn . Just $
                "lower bound must be greater than the upper bound, " <>
                "but the given bounds are [" <> pack (show lower) <> " .. " <>
                pack (show upper)
            ]

    TooManyRoles name amounts ->
        flip errorReport [] $
            "the compartment " <> name <> " contains " <>
            list "and" (fmap (\(TypeName tyName, amount) ->
                pack (show amount) <>
                " more role" <> (if amount > 1 then "s" else "") <>
                " of type " <> tyName <> " than allowed") amounts)


    DivisionByZero ->
        errorReport "division by zero"
            [ errorPart rgn $ Just
                "division by zero occurred while evaluating this expression"
            ]

    NotConstant ->
        errorReport "expression is not constant" [ errorPart rgn Nothing ]

    InvalidUpperBound len ->
        errorReport "invalid array size"
            [ errorPart rgn . Just $
                "array must have at least size 1, but the given size is " <>
                Text.pack (show len)
            ]

    IndexOutOfBounds (lower, upper) idx ->
        errorReport "index out of bounds"
            [ errorPart rgn . Just $
                "array has bounds [" <> pack (show lower) <> " .. " <>
                pack (show upper) <> "] but the index is " <>
                pack (show idx)
            ]

    ExceededDepth ->
        errorReport "exceeded maximum recursion depth"
            [ errorPart rgn . Just $
                "exceeded recursion depth evaluating this expression"
            ]


throw :: MonadError Error m => Region -> ErrorDesc -> m a
throw rgn = throwError . Error rgn


list :: Text -> [Text] -> Text
list _ []     = Text.empty
list _ [x]    = x
list c [x, y] = x <> " " <> c <> " " <> y
list c (x:xs) = x <> ", " <> list c xs
