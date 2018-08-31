{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}


module Rbsc.Translator.Coordinator
    ( trnsCoordinators
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import           Data.Set         (Set)
import qualified Data.Set         as Set

import qualified Language.Prism as Prism


import Rbsc.Data.Action
import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.Name
import Rbsc.Data.Some
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Alphabet
import Rbsc.Translator.Binding
import Rbsc.Translator.Command
import Rbsc.Translator.Expr
import Rbsc.Translator.Internal
import Rbsc.Translator.Variable

import Rbsc.Util.NameGen


trnsCoordinators
    :: ComponentTypes
    -> System
    -> BindingInfo
    -> Alphabets
    -> [TCoordinator Elem]
    -> Translator [Prism.Module]
trnsCoordinators compTys sys bi as = traverse (trnsCoordinator roleActMap)
  where
    roleActMap  = roleActions reqActss roleAs
    reqActss    = Map.mapWithKey getRequired roleAs
    getRequired = requiredActionsOfRole bi as
    roleAs      = Map.restrictKeys as roleCompNames
    roleCompNames =
        Map.keysSet (Map.filter (isRoleType compTys) (view instances sys))


trnsCoordinator
    :: Map RoleName (Set RoleAction)
    -> TCoordinator Elem
    -> Translator Prism.Module
trnsCoordinator roleActMap Coordinator{..} = do
    ident <- newNameFrom "coordinator"
    vars' <- trnsGlobalVars coordVars
    cmds' <- concat <$>
        traverse (trnsCoordCommand valuations roleActs . getElem) coordCommands

    alph <- fmap (Set.fromList . catMaybes) (traverse action coordCommands)

    let alph'   = Set.toList alph ++ playAlphabet
        alphCmd = if null playAlphabet
                      then []
                      else [alphabetCmd alph']

    return (Prism.Module ident vars' (cmds' ++ alphCmd))
  where
    valuations = allValuatations constrainedRoles

    alphabetCmd alph =
        Prism.Command alph Prism.ActionOpen (Prism.LitBool True) []

    playAlphabet = concatMap genPlayActs (Set.toList constrainedRoles)
    genPlayActs roleName =
        [playedActionIdent roleName, notPlayedActionIdent roleName]

    roleActs  = foldr compose Set.empty roleActss
    roleActss = fmap getRoleActs (Set.toList constrainedRoles)

    constrainedRoles = Set.unions (fmap rolesInConstraint constraints)
    constraints      = mapMaybe (coordConstraint . getElem) coordCommands

    getRoleActs roleName = Map.findWithDefault Set.empty roleName roleActMap

    action :: TElem (TCoordCommand Elem) -> Translator (Maybe Prism.Ident)
    action (Elem CoordCommand{..}) = case coordAction of
        Just (Loc (SomeExpr (Literal act _) TyAction) _) -> do
            act' <- trnsQualified (trnsAction act)
            return (Just act')
        Just (Loc _ rgn) -> throw rgn NotConstant
        Nothing -> return Nothing


trnsCoordCommand
    :: [Valuation]
    -> Set RoleAction
    -> TCoordCommand Elem
    -> Translator [Prism.Command]
trnsCoordCommand valuations roleActs CoordCommand {..} = do
    grd'      <- trnsLSomeExpr Nothing coordGuard
    upds'     <- trnsUpdates Nothing coordUpdates

    multiActs <- case coordConstraint of
        Just (Loc (SomeExpr c TyBool) rgn) -> do
            sat <- satisfyingValuations valuations (Loc c rgn)
            let constrActs = filter (isAllowed sat) (Set.toList roleActs)
            constrActs' <- case coordAction of
                Just (Loc (SomeExpr (Literal act _) TyAction) _) ->
                    -- If an action is given, only keep the role actions
                    -- with the matching action.
                    return (filter ((act ==) . fst) constrActs)
                Just (Loc _ rgn') -> throw rgn' NotConstant
                Nothing           -> return constrActs
            traverse toMultiAction constrActs'
        Just _  -> error "trnsCoordCommand: type error"
        Nothing -> case coordAction of
            Just act -> do
                act' <- trnsActionExpr act
                return [[act']]
            Nothing -> return [[]]

    return (fmap (mkCommand grd' upds') multiActs)
  where
    mkCommand grd upds acts
        | null acts = Prism.Command [] Prism.ActionClosed grd upds
        | otherwise = Prism.Command acts Prism.ActionOpen grd upds


toMultiAction :: RoleAction -> Translator [Prism.Ident]
toMultiAction (act, annot) = do
    act' <- trnsQualified (trnsAction act)
    let annots' = fmap trnsAnnot annot
    return (act' : annots')
  where
    trnsAnnot = \case
        Positive roleName -> playedActionIdent roleName
        Negative roleName -> notPlayedActionIdent roleName


isAllowed :: [Valuation] -> RoleAction -> Bool
isAllowed vals roleAct = any (`allows` roleAct) vals


allows :: Valuation -> RoleAction -> Bool
allows val (_, annot) = all contained annot
  where
    contained :: RoleAnnot -> Bool
    contained = \case
        Positive roleName -> case Map.lookup roleName val of
            Just True -> True
            _         -> False
        Negative roleName -> case Map.lookup roleName val of
            Just False -> True
            _          -> False


data RoleAnnot
    = Positive !RoleName
    | Negative !RoleName
    deriving (Eq, Ord, Show)


type RoleAction = (Action, [RoleAnnot])


roleActions
    :: Map RoleName (Set Action) -> Alphabets -> Map RoleName (Set RoleAction)
roleActions reqActss = Map.mapWithKey $ \name alph ->
    roleActionsOfRole (Map.findWithDefault Set.empty name reqActss) name alph


roleActionsOfRole :: Set Action -> RoleName -> Alphabet -> Set RoleAction
roleActionsOfRole reqActs roleName =
    Set.fromList . concatMap roleAction . Set.toList
  where
    roleAction (Loc act _, _)
        | act `Set.member` reqActs =
            [ (act, [Positive roleName])
            , (act, [Negative roleName])
            ]
        | otherwise =
            [ (act, [Positive roleName]) ]


compose :: Set RoleAction -> Set RoleAction -> Set RoleAction
compose x y = fromMap (Map.unionWith combine (toMap x) (toMap y))
  where
    combine annots1 annots2 =
        [ annot1 ++ annot2 | annot1 <- annots1, annot2 <- annots2 ]
    fromMap =
        Set.fromList
            . concatMap (\(act, annots) -> fmap ((,) act) annots)
            . Map.assocs
    toMap = Map.fromListWith (++) . fmap (over _2 (: [])) . Set.toList


type Valuation = Map RoleName Bool


satisfyingValuations
    :: MonadEval r m => [Valuation] -> Loc (Expr Bool) -> m [Valuation]
satisfyingValuations valuations constraint = filterM satisfying valuations
  where
    satisfying val = eval (fmap (substituteRoles val) constraint)

    substituteRoles :: Valuation -> Expr t -> Expr t
    substituteRoles val = transformExpr $ \case
        IsPlayed (Literal comp (TyComponent _)) ->
            case Map.lookup (view compName comp) val of
                Just b -> Literal b TyBool
                Nothing -> error $
                    "satisfyingValuations: " ++ show comp ++ " not found"
        e -> e


allValuatations :: Set RoleName -> [Valuation]
allValuatations = go . Set.toList
  where
    go (roleName : roleNames) = do
        val <- go roleNames
        b <- [True, False]
        return (Map.insert roleName b val)
    go [] = return Map.empty


rolesInConstraint :: LSomeExpr -> Set RoleName
rolesInConstraint (Loc (SomeExpr e _) _) =
    Set.fromList (mapMaybe getRole (universeExpr e))
  where
    getRole :: Some Expr -> Maybe RoleName
    getRole = \case
        Some (IsPlayed (Literal comp _)) -> Just (view compName comp)
        _ -> Nothing
