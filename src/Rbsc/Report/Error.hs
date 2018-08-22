{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Representation of model errors.
module Rbsc.Report.Error where


import Control.Lens
import Control.Monad.Except

import           Data.Text (Text, pack)
import qualified Data.Text as Text


import Rbsc.Data.Name

import           Rbsc.Report        hiding (errorReport)
import qualified Rbsc.Report        as Report
import           Rbsc.Report.Region


data Error
    = LocError !LocError
    | NoLocError !NoLocErrorDesc
    deriving (Eq, Show)


data LocError = MkLocError
    { _errorRegion :: !Region
    , _errorDesc   :: !LocErrorDesc
    } deriving (Eq, Show)


locError :: Region -> LocErrorDesc -> Error
locError rgn desc = LocError (MkLocError rgn desc)


data LocErrorDesc
    -- syntax errors
    = ParseError !Text
    | DuplicateModule !Region
    | UndefinedModule

    -- identifier errors
    | UndefinedType
    | DuplicateType !Region
    | NonRoleInCompartment
    | UndefinedIdentifier
    | DuplicateIdentifier !Region
    | CyclicDefinition !Text [Region]

    -- semantic errors
    | TypeError [Text] !Text
    | NotComparable !Text
    | UndefinedMember [TypeName] !Name
    | ConflictingMemberTypes !Name !TypeName !Text !TypeName !Text
    | SelfOutsideImpl
    | PlayerOutsideImpl
    | PlayerOutsideRole
    | IndexOutsideImpl
    | NotAnArray !Text
    | NotAFunction !Text
    | WrongNumberOfArguments !Int !Int
    | NotARole
    | CannotBePlayed !Text !TypeName
    | NoPossiblePlayers
    | NotACompartment
    | InvalidBinding !TypeName !TypeName
    | RoleAlreadyBound !Region
    | InvalidLowerBound !Int
    | InvalidCardinalities !Int !Int
    | InvalidOverrideAction
    | IncompatibleRoles !Name !Region !Name !Name !Text

    -- evaluation errors
    | DivisionByZero
    | NotConstant
    | InvalidUpperBound !Int
    | IndexOutOfBounds (Int, Int) !Int
    | ExceededDepth
    | HasNoPlayer !Name

    -- translation errors
    | TranslationNotSupported !Text
    | UndefinedPlayer !Name
    | NonIndexedComponent !Name
    deriving (Eq, Show)


data NoLocErrorDesc
    = TooManyRoles !Name [(TypeName, Int)]
    | NoSystems
    deriving (Eq, Show)


makePrisms ''Error
makeLenses ''LocError
makePrisms ''LocErrorDesc
makePrisms ''NoLocErrorDesc


toReport :: Error -> Report
toReport = \case
    LocError (MkLocError rgn desc) -> locReport rgn desc
    NoLocError desc                -> noLocReport desc


locReport :: Region -> LocErrorDesc -> Report
locReport rgn = \case
    ParseError err ->
        errorReport "syntax error" [errorPart rgn (Just err)]

    DuplicateModule first ->
        errorReport "duplicate module definition"
            [ errorPart rgn (Just "a module of the same name already exists")
            , hintPart first (Just "first definition was here")
            ]

    UndefinedModule ->
        errorReport "undefined module" [errorPart rgn Nothing]


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
        errorReport "type mismatch"
            [ errorPart rgn . Just $
                "expression has type: " <> actual <>
                "\nexpected type: " <> list "or" expected
            ]

    NotComparable ty ->
        errorReport "uncomparable values"
            [ errorPart rgn . Just $
                "values of type " <> ty <> " are not comparable"
            ]

    UndefinedMember [tyName] name ->
        errorReport "undefined local variable"
            [ errorPart rgn . Just $
                "local variable " <> name <> " is undefined for type " <>
                getTypeName tyName
            ]

    UndefinedMember tyNames name ->
        errorReport "undefined local variable"
            [ errorPart rgn . Just $
                "local variable " <> name <> " is undefined for the types " <>
                list "and" (fmap getTypeName tyNames)
            ]

    ConflictingMemberTypes name firstTyName firstTy secondTyName secondTy ->
        errorReport "conflicting variable types"
            [ errorPart rgn . Just $
                "local variable " <> name <> " of component type " <>
                getTypeName firstTyName <> " has type " <> firstTy <>
                ",\nbut " <> name <> " of component type " <>
                getTypeName secondTyName <> " has type " <> secondTy
            ]

    SelfOutsideImpl ->
        errorReport "self is undefined in this context"
            [ errorPart rgn . Just $
                "the self keyword can only be used in impl or module blocks"
            ]

    PlayerOutsideImpl ->
        errorReport "player is undefined in this context"
            [ errorPart rgn . Just $
                "the player keyword can only be used in impl or module blocks"
            ]

    PlayerOutsideRole ->
        errorReport "player is undefined in this context"
            [ errorPart rgn . Just $
                "the player keyword can only be used inside of role " <>
                "implementations"
            ]

    IndexOutsideImpl ->
        errorReport "index is undefined in this context"
            [ errorPart rgn . Just $
                "the index keyword can only be used inside impl or module blocks"
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

    CannotBePlayed ty tyName ->
        errorReport "component is not a role"
            [ errorPart rgn . Just $
                "component has type: " <> ty <>
                "\nbut " <> Text.pack (show tyName) <> " is not a role type"
            ]

    NoPossiblePlayers ->
        errorReport "no possible players"
            [ errorPart rgn . Just $
                "this component cannot play a role"
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

    RoleAlreadyBound first ->
        errorReport "role bound multiple times"
            [ errorPart rgn . Just $
                "this role has been bound already"
            , hintPart first (Just "the first binding was here")
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

    InvalidOverrideAction ->
        errorReport "invalid override action"
            [ errorPart rgn $ Just
                "override actions can only be used in role implementations"
            ]

    IncompatibleRoles first secondRgn second core act ->
        errorReport
            ("roles " <> first <> " and " <> second <> " bound to " <>
                core <> " are incompatible")
            [ errorPart rgn . Just $ "the overriding action " <> act <> " ..."
            , errorPart secondRgn (Just "... is also used here")
            ]


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

    HasNoPlayer name ->
        errorReport "component has no player"
            [ errorPart rgn . Just $
                "the component " <> name <> " does not have a player"
            ]


    TranslationNotSupported node ->
        errorReport "translation not supported"
            [ errorPart rgn . Just $
                "could not translate " <> node
            ]

    UndefinedPlayer name ->
        errorReport "undefined player"
            [ errorPart rgn . Just $
                "player is undefined for unbound role " <> name
            ]

    NonIndexedComponent name ->
        errorReport "component is not an array element"
            [ errorPart rgn . Just $
                "component " <> name <> " does not have an index"
            ]


noLocReport :: NoLocErrorDesc -> Report
noLocReport = \case
    TooManyRoles name amounts ->
        flip errorReport [] $
            "the compartment " <> name <> " contains " <>
            list "and" (fmap (\(TypeName tyName, amount) ->
                pack (show amount) <>
                " more role" <> (if amount > 1 then "s" else "") <>
                " of type " <> tyName <> " than allowed") amounts)

    NoSystems ->
        errorReport
            "there is no system instance satisfying the defined constraints"
            []


throw :: MonadError Error m => Region -> LocErrorDesc -> m a
throw rgn = throwError . LocError . MkLocError rgn


throwNoLoc :: MonadError Error m => NoLocErrorDesc -> m a
throwNoLoc = throwError . NoLocError


errorReport :: Text -> [Part] -> Report
errorReport title = Report.errorReport ("error: " <> title)


list :: Text -> [Text] -> Text
list _ []     = Text.empty
list _ [x]    = x
list c [x, y] = x <> " " <> c <> " " <> y
list c (x:xs) = x <> ", " <> list c xs
