{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}


module Rbsc.Translator.Internal
    ( Translator
    , runTranslator

    , TranslatorInfo
    , RolePlayingGuards
    , roleGuards
    , ObservedRoles
    , observedRoles
    , hasObservedRoles

    , TranslatorState
    , Identifiers

    , trnsQualified
    , trnsAction
    , overridePrefix
    , overrideActionIdent
    , playedActionIdent
    , notPlayedPrefix
    , notPlayedActionIdent
    , trnsComponentName
    , indexedNames
    , indexedExprs
    , addIndex
    , reduceLSomeExpr

    , rolePlayingGuards

    , addStepGuard
    , stepGuardName
    ) where


import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import           Data.Text          (isPrefixOf, pack)


import qualified Language.Prism as Prism


import Rbsc.Config

import Rbsc.Data.Action
import Rbsc.Data.ComponentType
import Rbsc.Data.Field
import Rbsc.Data.ModelInfo
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Util.NameGen


type TranslatorInfo =
    RolePlayingGuards :&:
    ObservedRoles :&:
    ModelInfo :&:
    RecursionDepth


-- | Stores the conditions (as guards) under which a given role can execute
-- a given action.
type RolePlayingGuards = Map RoleName (Map (Maybe Action) (NonEmpty LSomeExpr))


roleGuards :: Lens' TranslatorInfo RolePlayingGuards
roleGuards = field


type ObservedRoles = [RoleName]


observedRoles :: Lens' TranslatorInfo ObservedRoles
observedRoles = field


hasObservedRoles :: MonadReader TranslatorInfo m => m Bool
hasObservedRoles = not . null <$> view observedRoles


type TranslatorState =
    NameGen :&:
    Identifiers


type Identifiers = Map Qualified Prism.Ident


idents :: Lens' TranslatorState Identifiers
idents = field


type Translator a = StateT TranslatorState (ReaderT TranslatorInfo Result) a


runTranslator :: TranslatorInfo -> Translator a -> Result a
runTranslator info m = runReaderT (evalStateT m initState) info
  where
    initState = mkNameGen id Set.empty :&: Map.empty



trnsQualified :: MonadState TranslatorState m => Qualified -> m Prism.Ident
trnsQualified qname = use (idents.at qname) >>= \case
    Just ident -> return ident
    Nothing    -> do
        -- A Qualified name in the original model is unique. However,
        -- translating a Qualified name into a PRISM identifier may lead to
        -- clashes. Example: `QlName "c_x"` and `QlMember (QlName "c") "x"`
        -- map to the same identifier "c_x". Therefore, we use the NameGen
        -- to generate unique identifiers.

        let ident = mkIdent qname
        ident' <- newNameFrom ident

        idents.at qname ?= ident'

        return ident'
  where
    mkIdent = \case
        QlName name         -> name
        QlMember inner name -> mkIdent inner <> "_" <> name
        QlIndex inner idx   -> mkIdent inner <> "_" <> pack (show idx)


trnsAction :: Action -> Qualified
trnsAction = \case
    Action name             -> QlName (quote name)
    LocalAction   comp name -> QlMember (QlName (trnsComponentName comp)) name
    IndexedAction act  idx  -> QlIndex (trnsAction act) idx
  where
    quote name
        | overridePrefix `isPrefixOf` name || notPlayedPrefix `isPrefixOf` name =
            "_" <> name
        | otherwise = name


overridePrefix :: Name
overridePrefix = "_ovr_"


overrideActionIdent :: RoleName -> Name
overrideActionIdent roleName = overridePrefix <> trnsComponentName roleName


playedActionIdent :: RoleName -> Name
playedActionIdent roleName = "_" <> trnsComponentName roleName


notPlayedPrefix :: Name
notPlayedPrefix = "_not_"


notPlayedActionIdent :: RoleName -> Name
notPlayedActionIdent roleName =
    notPlayedPrefix <> trnsComponentName roleName


trnsComponentName :: ComponentName -> Name
trnsComponentName (ComponentName name mIdx) = case mIdx of
    Just idx -> name <> "_" <> pack (show idx)
    Nothing  -> name


indexedNames :: Qualified -> Type t -> [Qualified]
indexedNames qname = go id
  where
    go :: (Qualified -> Qualified) -> Type t -> [Qualified]
    go f = \case
        TyArray size innerTy ->
            flip concatMap [0 .. size - 1] $ \i ->
                go ((`QlIndex` i) . f) innerTy
        _ -> [f qname]


indexedExprs :: LSomeExpr -> Type t -> [LSomeExpr]
indexedExprs e = go id
  where
    go :: (LSomeExpr -> LSomeExpr) -> Type t -> [LSomeExpr]
    go f = \case
        TyArray size innerTy ->
            flip concatMap [0 .. size - 1] $ \i ->
                go (addIndex i . f) innerTy
        _ -> [f e]


addIndex :: Int -> LSomeExpr -> LSomeExpr
addIndex i (Loc (SomeExpr e (TyArray s innerTy)) rgn) =
    case dictShow innerTy of
        Dict ->
            let e' = Index e (Just s) (Loc (Literal (fromIntegral i) TyInt) rgn)
            in Loc (SomeExpr e' innerTy) rgn
addIndex _ _ = error "trnsVarDecl: not an array"


reduceLSomeExpr :: MonadEval r m => LSomeExpr -> m LSomeExpr
reduceLSomeExpr (Loc (SomeExpr e ty) rgn) = do
    Loc e' _ <- reduce (Loc e rgn)
    return (Loc (SomeExpr e' ty) rgn)


-- | Returns a map for each role component that contains the guards of all
-- guarded commands in that component, keyed by their action label.
rolePlayingGuards
    :: (MonadReader r m, Has ComponentTypes r)
    => System
    -> Map ComponentName [TModuleInstance Elem]
    -> m (Map RoleName (Map (Maybe Action) (NonEmpty LSomeExpr)))
rolePlayingGuards sys modules = do
    compTys <- view componentTypes
    let modules' = Map.filterWithKey (\name _ -> isRole compTys name) modules
    return (Map.map (Map.unions . fmap guardsByAction) modules')
  where
    isRole compTys name = case view (instances.at name) sys of
        Just tyName -> isRoleType compTys tyName
        Nothing     -> error $ "roleGuards: " ++ show name ++ " not in system"

    guardsByAction
        :: TModuleInstance Elem -> Map (Maybe Action) (NonEmpty LSomeExpr)
    guardsByAction = Map.fromListWith (<>) .
        fmap (fromCommand . getElem) . bodyCommands . view miBody

    fromCommand :: TCommand Elem -> (Maybe Action, NonEmpty LSomeExpr)
    fromCommand Command{..} =
        let mAct = case cmdAction of
                Just (Loc (SomeExpr (Literal act _) TyAction) _) ->
                    Just act
                _ -> Nothing
        in (mAct, cmdGuard :| [])


-- | If there are observed roles, then add the @step@ guard to a given
-- command's guard expression.
addStepGuard :: Prism.Expr -> Translator Prism.Expr
addStepGuard g = do
    hasObs <- hasObservedRoles
    return $ if hasObs
        then case g of
            Prism.LitBool True  -> Prism.Ident stepGuardName
            Prism.LitBool False -> g
            _ -> Prism.BinaryOp (Prism.Ident stepGuardName) Prism.And g
        else g


-- | If there are any observed roles, the system may only take a transition
-- once the role activity variables have been reset. This is the name of
-- the formula that evaluates to true whenever the model can take
-- a transition.
stepGuardName :: Name
stepGuardName = "step"
