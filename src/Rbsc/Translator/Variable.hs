{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Variable where


import Control.Lens
import Control.Monad.Reader

import Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Expr
import Rbsc.Translator.Internal


trnsLocalVars ::
       (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => TypeName
    -> Name
    -> TInits
    -> m [Prism.Declaration]
trnsLocalVars typeName compName =
    fmap concat . traverse (trnsVarDecl (Just (typeName, compName)))


trnsVarDecl ::
       (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => Maybe (TypeName, Name)
    -> (Name, Maybe LSomeExpr)
    -> m [Prism.Declaration]
trnsVarDecl mComp (varName, mInit) =
    view (symbolTable.at scName) >>= \case
        Just (Some ty) -> do
            declTys <- go id id ty
            for declTys $ \(qname, ty', mInit') -> do
                ident <- trnsQualified qname
                mInit'' <-
                    _Just (trnsLSomeExpr mCompName <=< reduceLSomeExpr) mInit'
                return (Prism.Declaration ident ty' mInit'')
        Nothing -> error $
            "trnsVarDecl: " ++ show scName ++ "not in symbol table"
  where
    baseName = case mComp of
        Just (_, compName) -> QlMember (QlName compName) varName
        Nothing            -> QlName varName

    scName = case mComp of
        Just (typeName, _) -> ScopedName (Local typeName) varName
        Nothing            -> ScopedName Global varName

    mCompName = fmap snd mComp

    go :: (MonadReader r m, HasSymbolTable r, HasRangeTable r)
       => (Qualified -> Qualified)
       -> (LSomeExpr -> LSomeExpr)
       -> Type t
       -> m [(Qualified, Prism.DeclarationType, Maybe LSomeExpr)]
    go modifyName modifyInit = \case
        TyBool -> return [
            ( modifyName baseName
            , Prism.DeclTypeBool
            , fmap modifyInit mInit
            ) ]
        TyInt ->
            view (rangeTable.at scName) >>= \case
                Just (lower, upper) -> return [
                    ( modifyName baseName
                    , Prism.DeclTypeInt
                        (Prism.LitInt (fromIntegral lower))
                        (Prism.LitInt (fromIntegral upper))
                    , fmap modifyInit mInit
                    ) ]
                Nothing -> error $
                    "trnsVarDecl: " ++ show scName ++ "not in range table"
        TyArray (lower, upper) innerTy ->
            fmap concat . for [lower .. upper] $ \i ->
                go ((`QlIndex` i) . modifyName) (addIndex i . modifyInit) innerTy
        ty -> error $ "trnsVarDecl: illegal var type " ++ show ty

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
