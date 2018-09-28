{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiWayIf #-}


module Rbsc.Translator.Command
    ( trnsCommand
    , trnsActionExpr
    , trnsUpdates
    ) where


import Control.Applicative
import Control.Lens
import Control.Monad.Except

import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map

import qualified Language.Prism as Prism


import Rbsc.Data.Action
import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Warning
import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Expr
import Rbsc.Translator.Internal


trnsCommand
    :: Bool
    -> TypeName
    -> ComponentName
    -> TCommand Elem
    -> Translator Prism.Command
trnsCommand isRole typeName comp Command{..} = do
    grd'  <- trnsLSomeExpr (Just (typeName, comp)) cmdGuard
    upds' <- trnsUpdates (Just (typeName, comp)) cmdUpdates

    case cmdAction of
        Just act -> do
            act' <- trnsActionExpr act
            ract <- roleAct
            oact <- overrideAct
            let acts' = fmap Prism.Action (catMaybes [Just act', ract, oact])
            return (Prism.Command acts' Prism.ActionOpen grd' upds')
        Nothing ->
            return (Prism.Command [] Prism.ActionClosed grd' upds')
  where
    roleAct
        | isRole = do
            act <- trnsQualified (QlName (playedActionIdent comp))
            return (Just act)
        | otherwise = return Nothing

    overrideAct = case cmdActionKind of
        OverrideAction _ -> do
            act <- trnsQualified (QlName (overrideActionIdent comp))
            return (Just act)
        _ -> return Nothing


trnsActionExpr :: LSomeExpr -> Translator Prism.Ident
trnsActionExpr (Loc e rgn) = case e of
    SomeExpr (Literal act _) TyAction -> trnsQualified (trnsAction act)
    _                                 -> throw rgn NotConstant


trnsUpdates
    :: Maybe (TypeName, ComponentName)
    -> [TElem (TUpdate Elem)]
    -> Translator [Prism.Update]
trnsUpdates mComp = traverse (trnsUpdate mComp . getElem)


trnsUpdate
    :: Maybe (TypeName, ComponentName)
    -> TUpdate Elem
    -> Translator Prism.Update
trnsUpdate mComp Update{..} = Prism.Update <$>
    _Just (trnsLSomeExpr mComp) updProb <*>
    (concat <$> traverse (trnsAssignment mComp . getElem) updAssignments)


trnsAssignment
    :: Maybe (TypeName, ComponentName)
    -> TAssignment
    -> Translator [(Prism.Ident, Prism.Expr)]
trnsAssignment mComp (Assignment (Loc name _) idxs e@(Loc (SomeExpr _ ty) _)) = do
    checkOutOfRange mComp name e

    symTable <- view symbolTable
    let (baseName, varTy) = if
            | Just (typeName, comp) <- mComp
            , Just ty' <- view (at (ScopedName (Local typeName) name)) symTable ->
                (QlMember (QlName (trnsComponentName comp)) name, ty')
            | Just ty' <- view (at (ScopedName Global name)) symTable ->
                (QlName name, ty')
            | otherwise -> error $
                "trnsAssignment: " ++ show name ++ "not in symbol table"

    Some varTy' <- return varTy
    indexedBaseName <- trnsIndices baseName varTy' idxs

    let qnames = indexedNames indexedBaseName ty
        es'    = indexedExprs e ty

    for (zip qnames es') $ \(qname, e') -> do
        ident <- trnsQualified qname
        e''   <- trnsLSomeExpr mComp =<< reduceLSomeExpr e'
        return (ident, e'')


trnsIndices
    :: MonadError Error m => Qualified -> Type t -> [LSomeExpr] -> m Qualified
trnsIndices qname (TyArray size innerTy) (Loc (SomeExpr idx TyInt) rgn : idxs) =
    case idx of
        Literal i _
            | 0 <= i && i < size ->
                trnsIndices (QlIndex qname i) innerTy idxs
            | otherwise -> throw rgn (IndexOutOfBounds size i)
        _ -> throw rgn NotConstant
trnsIndices qname _ [] = return qname
trnsIndices _     _ _  = error "trnsIndices: type error"


checkOutOfRange :: Maybe (TypeName, ComponentName) -> Name -> LSomeExpr -> Translator ()
checkOutOfRange mComp name (Loc (SomeExpr (Literal x TyInt) TyInt) rgn) = do
    rt <- view rangeTable

    let rangeLocal = case mComp of
            Just (typeName, _) ->
                Map.lookup (ScopedName (Local typeName) name) rt
            Nothing -> Nothing
        rangeGlobal = Map.lookup (ScopedName Global name) rt

    case rangeLocal <|> rangeGlobal of
        Just (lower, upper)
            | x < lower || x > upper ->
                lift (lift (warn (OutOfRangeUpdate rgn (lower, upper) x)))
        _ -> return ()
checkOutOfRange _ _ _ = return ()
