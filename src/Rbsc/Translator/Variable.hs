{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Variable
    ( trnsGlobalVars
    , trnsLocalVars
    ) where


import Control.Lens
import Control.Monad.Reader

import Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Expr
import Rbsc.Translator.Internal


trnsGlobalVars :: TInits -> Translator [Prism.Declaration]
trnsGlobalVars = trnsVarDecls Nothing


trnsLocalVars
    :: TypeName -> ComponentName -> TInits -> Translator [Prism.Declaration]
trnsLocalVars typeName compName = trnsVarDecls (Just (typeName, compName))


trnsVarDecls
    :: Maybe (TypeName, ComponentName)
    -> TInits
    -> Translator [Prism.Declaration]
trnsVarDecls mComp = fmap concat . traverse (trnsVarDecl mComp)


trnsVarDecl
    :: Maybe (TypeName, ComponentName)
    -> (Name, Maybe LSomeExpr)
    -> Translator [Prism.Declaration]
trnsVarDecl mComp (varName, mInit) =
    view (symbolTable.at scName) >>= \case
        Just (Some ty) -> do
            baseTy' <- baseType ty
            let qnames  = indexedNames baseName ty
                mInits' = case mInit of
                    Just e -> fmap Just (indexedExprs e ty)
                    Nothing -> repeat Nothing

            for (zip qnames mInits') $ \(qname, mInit') -> do
                ident <- trnsQualified qname
                mInit'' <-
                    _Just (trnsLSomeExpr mComp <=< reduceLSomeExpr) mInit'
                return (Prism.Declaration ident baseTy' mInit'')

        Nothing -> error $
            "trnsVarDecl: " ++ show scName ++ "not in symbol table"
  where
    baseName = case mComp of
        Just (_, compName) ->
            QlMember (QlName (trnsComponentName compName)) varName
        Nothing -> QlName varName

    scName = case mComp of
        Just (typeName, _) -> ScopedName (Local typeName) varName
        Nothing            -> ScopedName Global varName

    baseType ::
       (MonadReader r m, HasRangeTable r) => Type t -> m Prism.DeclarationType
    baseType = \case
        TyBool -> return Prism.DeclTypeBool
        TyInt  -> view (rangeTable.at scName) >>= \case
            Just (lower, upper) -> return (Prism.DeclTypeInt
                (Prism.LitInt (fromIntegral lower))
                (Prism.LitInt (fromIntegral upper)))
            Nothing -> error $
                "trnsVarDecl: " ++ show scName ++ "not in range table"
        TyArray _ innerTy -> baseType innerTy
        ty -> error $ "trnsVarDecl: illegal var type " ++ show ty
