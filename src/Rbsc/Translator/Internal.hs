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
    , overrideActionIdent
    , playedActionIdent
    , notPlayedActionIdent
    , indexedNames
    , indexedExprs
    , addIndex
    , reduceLSomeExpr
    ) where


import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Map.Strict     as Map
import Data.Text           (pack, replace)
import qualified Data.Set as Set


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
        QlName name         -> removeBrackets name
        QlMember inner name -> mkIdent inner <> "_" <> name
        QlIndex inner idx   -> mkIdent inner <> "_" <> pack (show idx)

    removeBrackets = replace "[" "_" . replace "]" ""


trnsAction :: Action -> Qualified
trnsAction = \case
    Action name           -> QlName name
    LocalAction comp name -> QlMember (QlName comp) name
    IndexedAction act idx -> QlIndex (trnsAction act) idx


overrideActionIdent :: RoleName -> Name
overrideActionIdent roleName = "ovr_" <> roleName


playedActionIdent :: RoleName -> Name
playedActionIdent = id


notPlayedActionIdent :: RoleName -> Name
notPlayedActionIdent roleName = "not_" <> roleName


indexedNames :: Qualified -> Type t -> [Qualified]
indexedNames qname = go id
  where
    go :: (Qualified -> Qualified) -> Type t -> [Qualified]
    go f = \case
        TyArray (lower, upper) innerTy ->
            flip concatMap [lower .. upper] $ \i ->
                go ((`QlIndex` i) . f) innerTy
        _ -> [f qname]


indexedExprs :: LSomeExpr -> Type t -> [LSomeExpr]
indexedExprs e = go id
  where
    go :: (LSomeExpr -> LSomeExpr) -> Type t -> [LSomeExpr]
    go f = \case
        TyArray (lower, upper) innerTy ->
            flip concatMap [lower .. upper] $ \i ->
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