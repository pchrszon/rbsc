{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


module Rbsc.Translator.Internal
    ( Translator
    , runTranslator
    , TranslatorState(..)

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
    ) where


import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Text       (pack, isPrefixOf)


import qualified Language.Prism as Prism


import Rbsc.Data.Action
import Rbsc.Data.Info
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Util.NameGen


data TranslatorState = TranslatorState
    { _tsNameGen :: NameGen
    , _tsIdents  :: !(Map Qualified Prism.Ident)
    }

makeLenses ''TranslatorState

instance HasNameGen TranslatorState where
    nameGen = tsNameGen


type Translator a = StateT TranslatorState (ReaderT Info Result) a


runTranslator :: Info -> Translator a -> Result a
runTranslator info m = runReaderT (evalStateT m initState) info
  where
    initState = TranslatorState (mkNameGen id Set.empty) Map.empty



trnsQualified :: MonadState TranslatorState m => Qualified -> m Prism.Ident
trnsQualified qname = use (tsIdents.at qname) >>= \case
    Just ident -> return ident
    Nothing    -> do
        -- A Qualified name in the original model is unique. However,
        -- translating a Qualified name into a PRISM identifier may lead to
        -- clashes. Example: `QlName "c_x"` and `QlMember (QlName "c") "x"`
        -- map to the same identifier "c_x". Therefore, we use the NameGen
        -- to generate unique identifiers.

        let ident = mkIdent qname
        ident' <- newNameFrom ident

        tsIdents.at qname ?= ident'

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
overridePrefix = "ovr_"


overrideActionIdent :: RoleName -> Name
overrideActionIdent roleName = overridePrefix <> trnsComponentName roleName


playedActionIdent :: RoleName -> Name
playedActionIdent = trnsComponentName


notPlayedPrefix :: Name
notPlayedPrefix = "not_"


notPlayedActionIdent :: RoleName -> Name
notPlayedActionIdent roleName = notPlayedPrefix <> trnsComponentName roleName


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
addIndex i (Loc (SomeExpr e (TyArray _ innerTy)) rgn) =
    case dictShow innerTy of
        Dict ->
            let e' = Index e (Loc (Literal (fromIntegral i) TyInt) rgn)
            in Loc (SomeExpr e' innerTy) rgn
addIndex _ _ = error "trnsVarDecl: not an array"


reduceLSomeExpr :: MonadEval r m => LSomeExpr -> m LSomeExpr
reduceLSomeExpr (Loc (SomeExpr e ty) rgn) = do
    Loc e' _ <- reduce (Loc e rgn)
    return (Loc (SomeExpr e' ty) rgn)
