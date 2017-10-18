{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}


-- | User-defined natural types, role types and compartment types.
module Rbsc.ComponentType
    ( ComponentTypes
    , ComponentType(..)
    , fromDeclarations
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Foldable   (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import qualified Rbsc.Report.Error.Syntax as Syntax
import           Rbsc.Report.Region
import           Rbsc.Syntax.Declaration
import           Rbsc.Syntax.TypeLevel
import           Rbsc.Type


-- | User-defined component types indexed by their name.
type ComponentTypes = Map TypeName ComponentType


-- | Represents a user-defined component type.
data ComponentType
      -- | A natural type.
    = NaturalType
      -- | A role type with its set of possible player types.
    | RoleType (Set TypeName)
      -- | A compartment type with its list of required role types.
    | CompartmentType [TypeName]
    deriving (Show)


-- | Extract 'ComponentTypes' from a list of 'Declaration's.
fromDeclarations :: [Declaration] -> Either [Syntax.Error] ComponentTypes
fromDeclarations decls =
    let (types, errors) = convert decls
        moreErrors = validate types decls
        allErrors = errors ++ moreErrors
    in if null allErrors
           then Right types
           else Left allErrors


convert :: [Declaration] -> (ComponentTypes, [Syntax.Error])
convert decls =
    over _1 (fmap fst) . flip execState (Map.empty, []) . for_ decls $ \case
        DeclNaturalType (NaturalTypeDef (Loc name rgn)) ->
            insertType name NaturalType rgn
        DeclRoleType (RoleTypeDef (Loc name rgn) playerTyNames) ->
            insertType
                name
                (RoleType (Set.fromList (fmap unLoc playerTyNames)))
                rgn
        DeclCompartmentType (CompartmentTypeDef (Loc name rgn) roleTyNames) ->
            insertType name (CompartmentType (fmap unLoc roleTyNames)) rgn
        _ -> return ()
  where
    insertType ::
           TypeName
        -> ComponentType
        -> Region
        -> State (Map TypeName (ComponentType, Region), [Syntax.Error]) ()
    insertType name ty rgn =
        use (_1.at name) >>= \case
            Just (_, rgnFirst) -> throw (Syntax.DuplicateType rgn rgnFirst)
            Nothing -> _1.at name .= Just (ty, rgn)

    throw e = modifying _2 (++ [e])


validate :: ComponentTypes -> [Declaration] -> [Syntax.Error]
validate types = concatMap validateDecl
  where
    validateDecl = \case
        DeclNaturalType _ -> []
        DeclRoleType (RoleTypeDef _ playerTyNames) ->
            mapMaybe exists playerTyNames
        DeclCompartmentType (CompartmentTypeDef _ roleTyNames) ->
            mapMaybe isRoleType roleTyNames
        _ -> []

    exists (Loc tyName rgn)
        | Map.member tyName types = Nothing
        | otherwise = Just (Syntax.UndefinedType rgn)

    isRoleType (Loc tyName rgn) = case Map.lookup tyName types of
        Just (RoleType _) -> Nothing
        Just _            -> Just (Syntax.NonRoleInCompartment rgn)
        Nothing           -> Just (Syntax.UndefinedType rgn)
