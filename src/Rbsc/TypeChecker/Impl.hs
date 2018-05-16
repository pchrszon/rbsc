{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Rbsc.TypeChecker.Impl
    ( tcImpls
    , tcVarDecl
    ) where


import Control.Lens
import Control.Monad.Reader

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Rbsc.Data.Scope
import Rbsc.Data.Type

import Rbsc.Report.Region
import Rbsc.Report.Result

import           Rbsc.Syntax.Typed   hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Typed   as T
import           Rbsc.Syntax.Untyped hiding (Model (..), Type (..))
import qualified Rbsc.Syntax.Untyped as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal


tcImpls ::
       Map TypeName [UModuleBody] -> TypeChecker (Map TypeName [TModuleBody ElemMulti])
tcImpls = Map.traverseWithKey tc
  where
    tc tyName bs = localScope tyName (traverse tcModuleBody bs)


tcModuleBody :: UModuleBody -> TypeChecker (TModuleBody ElemMulti)
tcModuleBody ModuleBody{..} = ModuleBody
    <$> traverse tcVarDecl bodyVars
    <*> tcElemMultis tcCommand bodyCommands


tcCommand :: UCommand -> TypeChecker (TCommand ElemMulti)
tcCommand Command{..} = Command
    <$> traverse (\act -> (`withLocOf` act) <$> tcAction act) cmdAction
    <*> someExpr cmdGuard TyBool
    <*> tcElemMultis tcUpdate cmdUpdates


tcUpdate :: UUpdate -> TypeChecker (TUpdate ElemMulti)
tcUpdate Update {..} = Update
    <$> traverse (`someExpr` TyDouble) updProb
    <*> tcElemMultis tcAssignment updAssignments


tcAssignment :: UAssignment -> TypeChecker TAssignment
tcAssignment (Assignment (Loc name rgn) idxs e) = do
    varTy <- getIdentifierType name rgn
    (idxs', SomeType ty) <- tcIndices rgn idxs varTy
    e' <- someExpr e ty
    return (Assignment (Loc name rgn) idxs' e')


tcIndices ::
       Region -> [U.LExpr] -> SomeType -> TypeChecker ([T.LSomeExpr], SomeType)
tcIndices _ [] ty = return ([], ty)
tcIndices rgn (i:is) (SomeType (TyArray _ elemTy)) = do
    i' <- someExpr i TyInt
    (is', ty') <- tcIndices rgn is (SomeType elemTy)
    return (i' : is', ty')
tcIndices rgn (_:_) (SomeType ty) = throwOne rgn (NotAnArray (renderType ty))


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
        body' <- local (over boundVars ((var, varTy) :)) $ tcElemMultis tc body
        return (Loop (Loc var rgn) qdTy' body')


tcVarDecl :: UVarDecl -> TypeChecker TInit
tcVarDecl (VarDecl (Loc name _) _ mInit) = do
    sc <- view scope
    view (symbolTable.at (ScopedName sc name)) >>= \case
        Just (SomeType ty) -> case mInit of
            Just e -> do
                e' <- e `hasType` ty
                return (name, Just (SomeExpr e' ty `withLocOf` e))
            Nothing -> return (name, Nothing)
        Nothing -> error ("tcVarDecl: " ++ show name ++ " not in symbol table")


someExpr :: LExpr -> Type t -> TypeChecker LSomeExpr
someExpr e ty = do
    e' <- e `hasType` ty
    return (SomeExpr e' ty `withLocOf` e)
