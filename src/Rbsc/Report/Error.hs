{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Representation of model errors.
module Rbsc.Report.Error where


import Control.Lens
import Control.Monad.Except

import Data.String

import Data.Text                 (Text, pack)
import Data.Text.Prettyprint.Doc hiding (list)


import Rbsc.Data.Name

import           Rbsc.Report        hiding (errorReport)
import qualified Rbsc.Report        as Report
import           Rbsc.Report.Region hiding (line)


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
    | UndefinedModule

    -- identifier errors
    | UndefinedType
    | DuplicateType !Region
    | NonRoleInCompartment
    | UndefinedIdentifier
    | DuplicateIdentifier !Name !Region
    | CyclicDefinition !Text [Region]

    -- semantic errors
    | TypeError [Text] !Text
    | NotComparable !Text
    | UndefinedMember [TypeName] !Name
    | ConflictingMemberTypes !Name !TypeName !Text !TypeName !Text
    | SelfOutsideImpl
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
    | EmptyGenArray !Int !Int
    | IllegalGlobalUpdate

    -- evaluation errors
    | DivisionByZero
    | NotConstant
    | IndexOutOfBounds !Int !Int
    | ExceededDepth
    | HasNoPlayer !Name
    | NonIndexedComponent !Name

    -- translation errors
    | TranslationNotSupported !Text
    | IllegalPlayingConstraint
    | OutOfRangeInit (Integer, Integer) Int
    deriving (Eq, Show)


data NoLocErrorDesc
    = TooManyRoles !Name [(TypeName, Int)]
    | NoSystems
    | EmptySystem
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
        errorReport "syntax error" [errorPart rgn (Just (pretty err))]

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

    DuplicateIdentifier name first ->
        errorReport ("duplicate definition of " <> name)
            [ errorPart rgn
                (Just "an identifier with this name already exists")
            , hintPart first (Just "first definition was here")
            ]

    CyclicDefinition construct rgns ->
        errorReport "cyclic definition" $
            errorPart rgn (Just $
                "this" <+> pretty construct <+>
                "is defined in terms of itself") :
            fmap (`hintPart` Nothing) rgns

    TypeError expected actual ->
        errorReport "type mismatch"
            [ errorPart rgn . Just $
                "expression has type:" <+> prettyIdent actual <> line <>
                "expected type:" <+> list "or" (prettyIdents expected)
            ]

    NotComparable ty ->
        errorReport "uncomparable values"
            [ errorPart rgn . Just $
                "values of type" <+> prettyIdent ty <+> "are not comparable"
            ]

    UndefinedMember [tyName] name ->
        errorReport "undefined local variable"
            [ errorPart rgn . Just $
                "local variable" <+> prettyIdent name <+>
                "is undefined for type" <+> prettyIdent (getTypeName tyName)
            ]

    UndefinedMember tyNames name ->
        errorReport "undefined local variable"
            [ errorPart rgn . Just $
                "local variable" <+> prettyIdent name <+>
                "is undefined for the types " <>
                list "and" (fmap (prettyIdent . getTypeName) tyNames)
            ]

    ConflictingMemberTypes name firstTyName firstTy secondTyName secondTy ->
        errorReport "conflicting variable types"
            [ errorPart rgn . Just $
                "local variable" <+> prettyIdent name <+>
                "of component type " <>
                prettyIdent (getTypeName firstTyName) <+> "has type" <+>
                prettyIdent firstTy <> "," <> line <>
                "but" <+> prettyIdent name <+> "of component type" <+>
                prettyIdent (getTypeName secondTyName) <+> "has type" <+>
                prettyIdent secondTy
            ]

    SelfOutsideImpl ->
        errorReport "self is undefined in this context"
            [ errorPart rgn . Just $
                "the self keyword can only be used in impl or module blocks"
            ]

    NotAnArray actual ->
        errorReport "not an array"
            [ errorPart rgn . Just $
                "expression has type:" <+> prettyIdent actual <> line <>
                "expected type: array"
            ]

    NotAFunction ty ->
        errorReport "not a function"
            [ errorPart rgn . Just $
                "this is not a function" <> line <> "expression has type:" <+>
                prettyIdent ty
            ]

    WrongNumberOfArguments expected actual ->
        errorReport "wrong number of arguments"
            [ errorPart rgn . Just $
                "arguments given:" <+> pretty actual <> line <>
                "expected:" <+> pretty expected
            ]

    NotARole ->
        errorReport "component is not a role"
            [ errorPart rgn . Just $
                "only roles can be bound to players and" <> line <>
                "be contained in compartments"
            ]

    CannotBePlayed ty tyName ->
        errorReport "component is not a role"
            [ errorPart rgn . Just $
                "component has type:" <+> prettyIdent ty <> line <>
                "but" <+> prettyIdent (getTypeName tyName) <+>
                "is not a role type"
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
                "a role of type" <+> prettyIdent (getTypeName roleTyName) <+>
                "cannot be bound to a player of type" <+>
                prettyIdent (getTypeName playerTyName)
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
                "lower bound must be greater or equal 0," <+>
                "but the given bound is" <+> pretty lower
            ]

    InvalidCardinalities lower upper ->
        errorReport "invalid cardinalities"
            [ errorPart rgn . Just $
                "lower bound must be greater than the upper bound," <+>
                "but the given bounds are [" <> pretty lower <+> ".." <+>
                pretty upper
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
            [ errorPart rgn . Just $
                "the overriding action" <+> prettyIdent act <+> "..."
            , errorPart secondRgn (Just "... is also used here")
            ]

    EmptyGenArray l u ->
        errorReport "empty array"
            [ errorPart rgn . Just $
                "this array is empty" <> line <>
                "its bounds are [" <> pretty l <+> ".." <+>
                pretty u <> "]"
            ]

    IllegalGlobalUpdate ->
        errorReport "illegal update of global variable"
            [ errorPart rgn . Just $
                "synchronous command cannot update global variable"
            ]


    DivisionByZero ->
        errorReport "division by zero"
            [ errorPart rgn $ Just
                "division by zero occurred while evaluating this expression"
            ]

    NotConstant ->
        errorReport "expression is not constant" [ errorPart rgn Nothing ]

    IndexOutOfBounds size idx ->
        errorReport "index out of bounds"
            [ errorPart rgn . Just $
                "array has size" <+> pretty size <+> "but the index is" <+>
                pretty idx
            ]

    ExceededDepth ->
        errorReport "exceeded maximum recursion depth"
            [ errorPart rgn . Just $
                "exceeded recursion depth evaluating this expression"
            ]

    HasNoPlayer name ->
        errorReport "component has no player"
            [ errorPart rgn . Just $
                "the component" <+> prettyIdent name <+>
                "does not have a player"
            ]

    NonIndexedComponent name ->
        errorReport "component is not an array element"
            [ errorPart rgn . Just $
                "component" <+> prettyIdent name <+> "does not have an index"
            ]


    TranslationNotSupported node ->
        errorReport "translation not supported"
            [ errorPart rgn . Just $ "could not translate" <+> pretty node
            ]

    IllegalPlayingConstraint ->
        errorReport "illegal playing constraint"
            [ errorPart rgn . Just $
                "role-playing constraints are not allowed" <> line <>
                "outside of coordinator commands"
            ]

    OutOfRangeInit (lower, upper) actual ->
        errorReport "invalid initial value"
            [ errorPart rgn . Just $
                "expression evaluates to " <> pretty actual <>
                ", but the variable has range [" <> pretty lower <> ".." <>
                pretty upper <> "]"
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

    EmptySystem ->
        errorReport
            "the system has no components"
            []


throw :: MonadError Error m => Region -> LocErrorDesc -> m a
throw rgn = throwError . LocError . MkLocError rgn


throwNoLoc :: MonadError Error m => NoLocErrorDesc -> m a
throwNoLoc = throwError . NoLocError


errorReport :: Text -> [Part] -> Report
errorReport title = Report.errorReport ("error: " <> title)


-- list :: Doc ann -> [Doc ann] -> Doc ann
list :: (Monoid a, IsString a) => a -> [a] -> a
list _ []     = mempty
list _ [x]    = x
list c [x, y] = x <> " " <> c <> " " <> y
list c (x:xs) = x <> ", " <> list c xs
