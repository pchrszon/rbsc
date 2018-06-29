{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Variable where


import Control.Lens
import Control.Monad.Reader

import Data.Semigroup
import Data.Text        (pack)
import Data.Traversable


import qualified Language.Prism as Prism


import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Syntax.Typed hiding (Type (..))


trnsLocalVars ::
       (MonadReader r m, HasSymbolTable r, HasRangeTable r)
    => TypeName
    -> Name
    -> TInits
    -> m [Prism.Declaration]
trnsLocalVars typeName compName =
    fmap concat . traverse (trnsVarDecl (Just (typeName, compName)))


-- TODO: set initial value
-- idea: generate Index expressions for array accesses in go function
trnsVarDecl ::
       (MonadReader r m, HasSymbolTable r, HasRangeTable r)
    => Maybe (TypeName, Name)
    -> (Name, Maybe LSomeExpr)
    -> m [Prism.Declaration]
trnsVarDecl mComp (varName, mInit) =
    view (symbolTable.at scName) >>= \case
        Just (Some ty) -> do
            declTys <- go id ty
            for declTys $ \(qname, ty') -> do
                ident <- trnsQualified qname
                return (Prism.Declaration ident ty' Nothing)
        Nothing -> error $
            "trnsVarDecl: " ++ show scName ++ "not in symbol table"
  where
    baseName = case mComp of
        Just (_, compName) -> QlMember (QlName compName) varName
        Nothing            -> QlName varName

    scName = case mComp of
        Just (typeName, _) -> ScopedName (Local typeName) varName
        Nothing            -> ScopedName Global varName

    go :: (MonadReader r m, HasSymbolTable r, HasRangeTable r)
       => (Qualified -> Qualified)
       -> Type t
       -> m [(Qualified, Prism.DeclarationType)]
    go f = \case
        TyBool -> return [(f baseName, Prism.DeclTypeBool)]
        TyInt ->
            view (rangeTable.at scName) >>= \case
                Just (lower, upper) -> return [
                    ( f baseName
                    , Prism.DeclTypeInt
                        (Prism.LitInt (fromIntegral lower))
                        (Prism.LitInt (fromIntegral upper)))]
                Nothing -> error $
                    "trnsVarDecl: " ++ show scName ++ "not in range table"
        TyArray (lower, upper) innerTy ->
            fmap concat . for [lower .. upper] $ \i ->
                go ((`QlIndex` i) . f) innerTy
        ty -> error $ "trnsVarDecl: illegal var type " ++ show ty


trnsQualified :: Monad m => Qualified -> m Prism.Ident
trnsQualified = return . go
  where
    go = \case
        QlName name         -> name
        QlMember inner name -> go inner <> "_" <> name
        QlIndex inner idx   -> go inner <> "_" <> pack (show idx)
