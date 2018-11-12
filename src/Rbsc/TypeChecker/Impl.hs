{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Rbsc.TypeChecker.Impl
    ( tcModuleInstances
    , tcVarDecl

    , tcUpdate
    , tcElemMultis
    , someExpr
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (isJust)


import Rbsc.Data.Action
import Rbsc.Data.ComponentType
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed   hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Untyped as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal

import Rbsc.Util (renderPretty, withConstants)


tcModuleInstances
    :: Map TypeName [UModuleInstance]
    -> TypeChecker (Map TypeName [TModuleInstance ElemMulti])
tcModuleInstances = Map.traverseWithKey tc
  where
    tc  :: TypeName
        -> [UModuleInstance]
        -> TypeChecker [TModuleInstance ElemMulti]
    tc tyName mis = localScope tyName (traverse tcModuleInstance mis)


tcModuleInstance :: UModuleInstance -> TypeChecker (TModuleInstance ElemMulti)
tcModuleInstance mi = do
    body' <- withConstants (view miArgs mi) (tcModuleBody (view miBody mi))
    return (set miBody body' mi)


tcModuleBody :: UModuleBody -> TypeChecker (TModuleBody ElemMulti)
tcModuleBody ModuleBody{..} = ModuleBody
    <$> traverse tcVarDecl bodyVars
    <*> tcElemMultis tcCommand bodyCommands


tcCommand :: UCommand -> TypeChecker (TCommand ElemMulti)
tcCommand Command{..} = do
    checkActionKind
    Command
        <$> traverse (\act -> (`withLocOf` act) <$> tcAction act) cmdAction
        <*> pure cmdActionKind
        <*> pure cmdActionIntent
        <*> someExpr cmdGuard TyBool
        <*> tcElemMultis (tcUpdate hasAction []) cmdUpdates
  where
    checkActionKind = case cmdActionKind of
        OverrideAction rgn -> view scope >>= \case
            Local tyName -> view (componentTypes.at tyName) >>= \case
                Just (RoleType _) -> return ()
                _ -> throw rgn InvalidOverrideAction
            Global -> throw rgn InvalidOverrideAction
        NormalAction -> return ()

    hasAction = isJust cmdAction


tcUpdate :: Bool -> [Name] -> UUpdate -> TypeChecker (TUpdate ElemMulti)
tcUpdate hasAction ownVars Update {..} = Update
    <$> traverse (`someExpr` TyDouble) updProb
    <*> tcElemMultis (tcAssignment hasAction ownVars) updAssignments


tcAssignment :: Bool -> [Name] -> UAssignment -> TypeChecker TAssignment
tcAssignment hasAction ownVars (Assignment (Loc name rgn) idxs e) = do
    -- check for illegal update of global variable
    symTable <- view symbolTable
    when (hasAction && (name `notElem` ownVars)) $
        view scope >>= \case
            Local tyName
                | not (isLocalSymbol symTable tyName name) ->
                    throw rgn IllegalGlobalUpdate
            _ -> return ()

    varTy <- getIdentifierType name rgn
    (idxs', Some ty) <- tcIndices rgn idxs varTy
    e' <- someExpr e ty
    return (Assignment (Loc name rgn) idxs' e')


tcIndices ::
       Region
    -> [U.LExpr]
    -> Some Type
    -> TypeChecker ([T.LSomeExpr], Some Type)
tcIndices _ [] ty = return ([], ty)
tcIndices rgn (i:is) (Some (TyArray _ elemTy)) = do
    i' <- someExpr i TyInt
    (is', ty') <- tcIndices rgn is (Some elemTy)
    return (i' : is', ty')
tcIndices rgn (_:_) (Some ty) = throw rgn (NotAnArray (renderPretty ty))


tcElemMultis ::
       (a -> TypeChecker b)
    -> [UElemMulti a]
    -> TypeChecker [TElemMulti b]
tcElemMultis tc = traverse (tcElemMulti tc)


tcElemMulti ::
       (a -> TypeChecker b) -> UElemMulti a -> TypeChecker (TElemMulti b)
tcElemMulti tc =
    \case
        ElemSingle x  -> ElemSingle <$> tc x
        ElemLoop l    -> ElemLoop <$> tcLoop l
        ElemIf e body -> ElemIf <$> someExpr e TyBool <*> tcElemMultis tc body
  where
    tcLoop (Loop (Loc var rgn) qdTy body) = do
        (qdTy', varTy) <- tcQuantifiedType qdTy
        body' <- local (over boundVars ((var, varTy) :)) (tcElemMultis tc body)
        return (Loop (Loc var rgn) qdTy' body')


tcVarDecl :: UVarDecl -> TypeChecker TInit
tcVarDecl (VarDecl (Loc name _) _ mInit) = do
    sc <- view scope
    view (symbolTable.at (ScopedName sc name)) >>= \case
        Just (Some ty) -> case mInit of
            Just e -> do
                e' <- e `hasType` ty
                return (name, Just (SomeExpr e' ty `withLocOf` e))
            Nothing -> return (name, Nothing)
        Nothing -> error ("tcVarDecl: " ++ show name ++ " not in symbol table")


someExpr :: LExpr -> Type t -> TypeChecker LSomeExpr
someExpr e ty = do
    e' <- e `hasType` ty
    return (SomeExpr e' ty `withLocOf` e)
