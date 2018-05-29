{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Reduction of expressions in modules. This includes removal of function
-- calls, quantification and array accesses.
module Rbsc.Translator.Reduction where


import Control.Lens
import Control.Monad.Reader

import Data.Foldable
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set


import Rbsc.Data.Component
import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Expr.Typed
import Rbsc.Syntax.Typed      hiding (Type (..))


data Variable = Variable
    { varName  :: !Name
    , varScope :: VariableScope
    } deriving (Eq, Show)


data VariableScope
    = GlobalVar
    | LocalVar !TypeName !Name
    deriving (Eq, Show)


data TypedVariable = TypedVariable Variable (Some Type) deriving (Show)

instance Eq TypedVariable where
    TypedVariable (Variable l _) _ == TypedVariable (Variable r _) _ = l == r

instance Ord TypedVariable where
    compare (TypedVariable (Variable l _) _) (TypedVariable (Variable r _) _) =
        compare l r


data Range
    = BoolRange
    | IntRange (Int, Int)
    deriving (Eq, Show)


removeVarIndicesInBody ::
       (MonadReader r m, HasConstants r, HasSymbolTable r, HasRangeTable r)
    => Component
    -> TModuleBody Elem
    -> m (TModuleBody Elem)
removeVarIndicesInBody comp (ModuleBody vars cmds) = do
    cmds' <- concat <$> traverse removeIndices cmds
    return (ModuleBody vars cmds')
  where
    removeIndices (Elem cmd) = fmap Elem <$> removeVariableIndices comp cmd


removeVariableIndices ::
       (MonadReader r m, HasConstants r, HasSymbolTable r, HasRangeTable r)
    => Component
    -> TCommand Elem
    -> m [TCommand Elem]
removeVariableIndices comp cmd = do
    ranges <- getIndexRanges comp cmd
    return (fmap remove (variableValues ranges))
  where
    remove vals =
        let cmd' = substituteVariables vals cmd
            g = (case cmdGuard cmd' of
                    Loc (SomeExpr e TyBool) _ -> e
                    _ -> error "removeVariableIndices: type error") :: Expr Bool
            g' = LogicOp And (valueGuard vals) g
        in cmd' { cmdGuard = SomeExpr g' TyBool `withLocOf` cmdGuard cmd }


valueGuard :: [(Variable, SomeExpr)] -> Expr Bool
valueGuard = \case
    []   -> Literal True
    vars -> foldr1 (LogicOp And) (fmap toGuard vars)
  where
    toGuard :: (Variable, SomeExpr) -> Expr Bool
    toGuard (Variable name sc, SomeExpr val ty) =
        let ident = case sc of
                GlobalVar -> Identifier name ty
                LocalVar tyName cName ->
                    Member (Identifier cName (TyComponent [tyName])) name ty
        in case checkEq ty of
            Just Dict -> EqOp Eq ident val
            Nothing   -> error "variableGuard: type error"


variableValues :: [(Variable, Range)] -> [[(Variable, SomeExpr)]]
variableValues = (traverse._2) getValue
  where
    getValue = \case
        BoolRange -> do
            val <- [False, True]
            return (SomeExpr (Literal val) TyBool)
        IntRange (lower, upper) -> do
            val <- fromIntegral <$> [lower .. upper]
            return (SomeExpr (Literal val) TyInt)


substituteVariables :: HasExprs a => [(Variable, SomeExpr)] -> a -> a
substituteVariables vals x = foldr substituteVariable x vals


substituteVariable :: HasExprs a => (Variable, SomeExpr) -> a -> a
substituteVariable (Variable name sc, SomeExpr e ty) =
    transformExprs substitute
  where
    substitute :: Expr t -> Expr t
    substitute e' = case e' of
        Identifier name' ty'
            | name' == name -> case typeEq ty ty' of
                Just Refl -> e
                Nothing   -> error "substituteVariable: type error"
        Member (Identifier cName' _) name' ty' -> case sc of
            LocalVar _ cName
                | name' == name && cName' == cName -> case typeEq ty ty' of
                    Just Refl -> e
                    Nothing   -> error "substituteVariable: type error"
            _ -> e'
        _ -> e'


getIndexRanges ::
       (MonadReader r m, HasSymbolTable r, HasConstants r, HasRangeTable r)
    => Component
    -> TCommand Elem
    -> m [(Variable, Range)]
getIndexRanges comp cmd = do
    vars <- Set.unions <$> traverse (variables comp) (indexExprs cmd)
    getVariableRanges (toList vars)


-- | Retrieve all 'Expr's that appear as an index inside an 'Index' operator.
indexExprs :: TCommand Elem -> [Some Expr]
indexExprs cmd =
    mapMaybe indexExpr (universeExprs cmd) ++
    concatMap assignmentIndexExprs assignments
  where
    assignments =
        concatMap (fmap getElem . updAssignments . getElem) (cmdUpdates cmd)

    indexExpr (Some e) = case e of
        Index _ (Loc idx _) -> Just (Some idx)
        _                   -> Nothing

    assignmentIndexExprs (Assignment _ idxs _) = fmap fromLSomeExpr idxs

    fromLSomeExpr (Loc (SomeExpr e _) _) = Some e


getVariableRanges ::
       (MonadReader r m, HasRangeTable r)
    => [TypedVariable]
    -> m [(Variable, Range)]
getVariableRanges = fmap catMaybes . traverse getRange
  where
    getRange ::
           (MonadReader r m, HasRangeTable r)
        => TypedVariable
        -> m (Maybe (Variable, Range))
    getRange (TypedVariable var (Some ty)) = case ty of
        TyBool -> return (Just (var, BoolRange))
        TyInt  -> do
            mr <- view (rangeTable.at (scopedName var))
            case mr of
                Nothing -> error $
                    "getVariableRanges: " ++ show var ++ " not in range table"
                Just range -> return (Just (var, IntRange range))
        _ -> return Nothing

    scopedName (Variable name sc) = case sc of
        GlobalVar         -> ScopedName Global name
        LocalVar tyName _ -> ScopedName (Local tyName) name


variables ::
       (MonadReader r m, HasSymbolTable r, HasConstants r)
    => Component
    -> Some Expr
    -> m (Set TypedVariable)
variables comp (Some e) =
    fmap (Set.fromList . catMaybes) (traverse variable (universeExpr e))
  where
    variable (Some e') = do
        symTable <- view symbolTable
        consts   <- view constants
        return $ case e' of
            Identifier name ty
                | has (at (ScopedName (Local tyName) name)._Just) symTable ->
                    Just (TypedVariable
                        (Variable name (LocalVar tyName (view compName comp)))
                        (Some ty))
                | has (at (ScopedName Global name)._Just) symTable &&
                  has (at name._Nothing) consts ->
                    Just (TypedVariable (Variable name GlobalVar) (Some ty))
            Member (Identifier cName (TyComponent (toList -> [tyName']))) name ty
                | has (at (ScopedName (Local tyName') name)._Just) symTable ->
                    Just (TypedVariable
                        (Variable name (LocalVar tyName' cName))
                        (Some ty))
            _ -> Nothing

    tyName = view compTypeName comp
