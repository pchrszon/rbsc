{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Internal
    ( trnsQualified
    , trnsAction
    , overrideActionIdent
    , indexedNames
    , indexedExprs
    , addIndex
    , reduceLSomeExpr
    ) where


import Data.Semigroup
import Data.Text      (pack, replace)


import qualified Language.Prism as Prism


import Rbsc.Data.Action
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))


trnsQualified :: Monad m => Qualified -> m Prism.Ident
trnsQualified = return . go
  where
    go = \case
        QlName name         -> removeBrackets name
        QlMember inner name -> go inner <> "_" <> name
        QlIndex inner idx   -> go inner <> "_" <> pack (show idx)

    removeBrackets = replace "[" "_" . replace "]" ""


trnsAction :: Action -> Qualified
trnsAction = \case
    Action name           -> QlName name
    LocalAction comp name -> QlMember (QlName comp) name
    IndexedAction act idx -> QlIndex (trnsAction act) idx


overrideActionIdent :: RoleName -> Prism.Ident
overrideActionIdent roleName = "override_" <> roleName


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
