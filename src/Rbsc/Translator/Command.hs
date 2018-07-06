{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
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

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Expr
import Rbsc.Translator.Internal


trnsCommand ::
       (MonadEval r m, HasSymbolTable r)
    => TypeName
    -> Name
    -> TCommand Elem
    -> m Prism.Command
trnsCommand typeName comp Command{..} = do
    act'  <- maybeToList <$> _Just trnsActionExpr cmdAction
    grd'  <- trnsLSomeExpr (Just comp) cmdGuard
    upds' <- traverse (trnsUpdate typeName comp . getElem) cmdUpdates
    return (Prism.Command act' Prism.ActionClosed grd' upds')


trnsActionExpr :: MonadError Error m => LSomeExpr -> m Prism.Ident
trnsActionExpr (Loc e rgn) = case e of
    SomeExpr (Literal act _) TyAction -> trnsQualified (trnsAction act)
    _ -> throw rgn NotConstant


trnsAction :: Action -> Qualified
trnsAction = \case
    Action name -> QlName name
    LocalAction comp name -> QlMember (QlName comp) name
    IndexedAction act idx -> QlIndex (trnsAction act) idx


trnsUpdate ::
       (MonadEval r m, HasSymbolTable r)
    => TypeName
    -> Name
    -> TUpdate Elem
    -> m Prism.Update
trnsUpdate typeName comp Update{..} = Prism.Update <$>
    _Just (trnsLSomeExpr (Just comp)) updProb <*>
    (concat <$> traverse (trnsAssignment typeName comp . getElem) updAssignments)


trnsAssignment ::
       (MonadEval r m, HasSymbolTable r)
    => TypeName
    -> Name
    -> TAssignment
    -> m [(Prism.Ident, Prism.Expr)]
trnsAssignment typeName comp (Assignment (Loc name _) idxs e@(Loc (SomeExpr _ ty) _)) = do
    symTable <- view symbolTable
    let (baseName, varTy) =
            case view (at (ScopedName (Local typeName) name)) symTable of
                Just ty' -> (QlMember (QlName comp) name, ty')
                Nothing -> case view (at (ScopedName Global name)) symTable of
                    Just ty' -> (QlName name, ty')
                    Nothing -> error $
                        "trnsAssignment: " ++ show name ++ "not in symbol table"
        mods = go id id ty

    Some varTy' <- return varTy
    indexedBaseName <- trnsIndices baseName varTy' idxs

    for mods $ \(modifyName, modifyExpr) -> do
        ident <- trnsQualified (modifyName indexedBaseName)
        e'    <- trnsLSomeExpr (Just comp) =<< reduceLSomeExpr (modifyExpr e)
        return (ident, e')
  where
    go :: (Qualified -> Qualified)
       -> (LSomeExpr -> LSomeExpr)
       -> Type t
       -> [(Qualified -> Qualified, LSomeExpr -> LSomeExpr)]
    go modifyName modifyExpr = \case
        TyArray (lower, upper) innerTy -> flip concatMap [lower .. upper] $ \i ->
            go ((`QlIndex` i) . modifyName) (addIndex i . modifyExpr) innerTy
        _ -> [(modifyName, modifyExpr)]


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
