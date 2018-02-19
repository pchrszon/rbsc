{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Representation of model errors.
module Rbsc.Report.Error where


import Control.Lens
import Control.Monad.Except

import Data.Monoid

import           Data.Text (Text)
import qualified Data.Text as Text

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

    | DivisionByZero
    | NotConstant
    | IndexOutOfBounds !Int !Int
    | ExceededDepth
    deriving (Eq, Show)


makeLenses ''Error
makePrisms ''ErrorDesc


toReport :: Error -> Report
toReport (Error rgn desc) = case desc of
    ParseError err ->
        Report "syntax error" [errorPart rgn (Just err)]

    UndefinedType ->
        Report "undefined type" [errorPart rgn Nothing]

    DuplicateType first ->
        Report "duplicate type definition"
            [ errorPart rgn (Just "a type of the same name already exists")
            , hintPart first (Just "first definition was here")
            ]

    NonRoleInCompartment ->
        Report "only roles can be contained in compartments"
            [ errorPart rgn (Just "this is not a role type")
            ]

    UndefinedIdentifier ->
        Report "undefined identifier" [errorPart rgn Nothing]

    DuplicateIdentifier first ->
        Report "duplicate definition"
            [ errorPart rgn
                (Just "an identifier with this name already exists")
            , hintPart first (Just "first definition was here")
            ]

    CyclicDefinition construct rgns ->
        Report "cyclic definition" $
            (errorPart rgn (Just $
                "this " <> construct <> " is defined in terms of itself")) :
            fmap (\r -> hintPart r Nothing) rgns

    TypeError expected actual ->
        Report "type error"
            [ errorPart rgn . Just $
                "expression has type: " <> actual <>
                "\nexpected type: " <> orList expected
            ]

    NotComparable ty ->
        Report "uncomparable values"
            [ errorPart rgn . Just $
                "values of type " <> ty <> " are not comparable"
            ]

    NotAnArray actual ->
        Report "not an array"
            [ errorPart rgn . Just $
                "expression has type: " <> actual <>
                "\nexpected type: array"
            ]

    NotAFunction ty ->
        Report "not a function"
            [ errorPart rgn . Just $
                "this is not a function\nexpression has type: " <> ty
            ]

    WrongNumberOfArguments expected actual ->
        Report "wrong number of arguments"
            [ errorPart rgn . Just $
                "arguments given: " <> Text.pack (show actual) <>
                "\nexpected: " <> Text.pack (show expected)
            ]

    DivisionByZero ->
        Report "division by zero"
            [ errorPart rgn $ Just
                "division by zero occurred while evaluating this expression"
            ]

    NotConstant ->
        Report "expression is not constant" [ errorPart rgn Nothing ]

    IndexOutOfBounds len idx ->
        Report "index out of bounds"
            [ errorPart rgn . Just $
                "array has size " <> Text.pack (show len) <>
                "but the index is " <> Text.pack (show idx)
            ]

    ExceededDepth ->
        Report "exceeded maximum recursion depth"
            [ errorPart rgn . Just $
                "exceeded recursion depth evaluating this expression"
            ]


throw :: MonadError Error m => Region -> ErrorDesc -> m a
throw rgn = throwError . Error rgn


orList :: [Text] -> Text
orList []     = Text.empty
orList [x]    = x
orList [x, y] = x <> " or " <> y
orList (x:xs) = x <> ", " <> orList xs
