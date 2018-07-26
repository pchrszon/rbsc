{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


module Rbsc.Translator.Command where


import Control.Lens
import Control.Monad.Except

import Data.Maybe
import Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.Action
import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Expr
import Rbsc.Translator.Internal


trnsCommand
    :: Bool -> TypeName -> Name -> TCommand Elem -> Translator Prism.Command
trnsCommand isRole typeName comp Command{..} = do
    grd'  <- trnsLSomeExpr (Just (typeName, comp)) cmdGuard
    upds' <- traverse (trnsUpdate typeName comp . getElem) cmdUpdates

    case cmdAction of
        Just act -> do
            act' <- trnsActionExpr act
            ract <- roleAct
            oact <- overrideAct
            let acts' = catMaybes [Just act', ract, oact]
            return (Prism.Command acts' Prism.ActionOpen grd' upds')
        Nothing ->
            return (Prism.Command [] Prism.ActionClosed grd' upds')
  where
    roleAct
        | isRole = do
            act <- trnsQualified (QlName comp)
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


trnsUpdate :: TypeName -> Name -> TUpdate Elem -> Translator Prism.Update
trnsUpdate typeName comp Update{..} = Prism.Update <$>
    _Just (trnsLSomeExpr (Just (typeName, comp))) updProb <*>
    (concat <$> traverse (trnsAssignment typeName comp . getElem) updAssignments)


trnsAssignment
    :: TypeName -> Name -> TAssignment -> Translator [(Prism.Ident, Prism.Expr)]
trnsAssignment typeName comp (Assignment (Loc name _) idxs e@(Loc (SomeExpr _ ty) _)) = do
    symTable <- view symbolTable
    let (baseName, varTy) =
            case view (at (ScopedName (Local typeName) name)) symTable of
                Just ty' -> (QlMember (QlName comp) name, ty')
                Nothing -> case view (at (ScopedName Global name)) symTable of
                    Just ty' -> (QlName name, ty')
                    Nothing -> error $
                        "trnsAssignment: " ++ show name ++ "not in symbol table"

    Some varTy' <- return varTy
    indexedBaseName <- trnsIndices baseName varTy' idxs

    let qnames = indexedNames indexedBaseName ty
        es'    = indexedExprs e ty

    for (zip qnames es') $ \(qname, e') -> do
        ident <- trnsQualified qname
        e''   <- trnsLSomeExpr (Just (typeName, comp)) =<< reduceLSomeExpr e'
        return (ident, e'')


trnsIndices :: MonadError Error m => Qualified -> Type t -> [LSomeExpr] -> m Qualified
trnsIndices qname (TyArray (lower, upper) innerTy) (Loc (SomeExpr idx TyInt) rgn : idxs) =
    case idx of
        Literal i _
            | lower <= i && i <= upper ->
                trnsIndices (QlIndex qname i) innerTy idxs
            | otherwise -> throw rgn (IndexOutOfBounds (lower, upper) i)
        _ -> throw rgn NotConstant
trnsIndices qname _ [] = return qname
trnsIndices _     _ _  = error "trnsIndices: type error"
