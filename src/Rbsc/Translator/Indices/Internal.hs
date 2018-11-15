{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Internal functions for the Indices module.
module Rbsc.Translator.Indices.Internal
    ( Variable(..)
    , VariableScope(..)
    , TypedVariable(..)
    , Range(..)

    , valueGuard
    , variableValues
    , substituteVariable
    , Updates
    , getIndexRanges
    , indexExprs
    , getVariableRanges
    , variables
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.Foldable
import           Data.Maybe
import           Data.Set      (Set)
import qualified Data.Set      as Set


import Rbsc.Data.Component
import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))


-- | A @Variable@ has a 'Name' and can be either global or local.
data Variable = Variable
    { _varName  :: !Name
    , _varScope :: VariableScope
    } deriving (Eq, Show)


-- | The scope of a variable.
data VariableScope
    = GlobalVar
    | LocalVar !TypeName !Name
    deriving (Eq, Show)


-- | A variable with its 'Type'.
data TypedVariable = TypedVariable Variable (Some Type) deriving (Show)

instance Eq TypedVariable where
    TypedVariable (Variable l _) _ == TypedVariable (Variable r _) _ = l == r

instance Ord TypedVariable where
    compare (TypedVariable (Variable l _) _) (TypedVariable (Variable r _) _) =
        compare l r


-- | The possible values a variable can have.
data Range
    = BoolRange
    | IntRange (Int, Int)
    deriving (Eq, Show)


-- | Construct a guard expression stating that each variable has the given
-- value.
valueGuard :: [(Variable, SomeExpr)] -> Expr Bool
valueGuard = \case
    []   -> Literal True TyBool
    vars -> foldr1 (LogicOp And) (fmap toGuard vars)
  where
    toGuard :: (Variable, SomeExpr) -> Expr Bool
    toGuard (Variable name sc, SomeExpr val ty) =
        let ident = case sc of
                GlobalVar -> Identifier name ty
                LocalVar tyName cName ->
                    Member (Identifier cName (TyComponent [tyName])) name ty
        in case checkEq ty of
            Just Dict -> EqOp Eq ty ident val
            Nothing   -> error "variableGuard: type error"


-- | Get a list of all possible combinations of assigning values to the
-- given list of variables.
variableValues :: [(Variable, Range)] -> [[(Variable, SomeExpr)]]
variableValues = (traverse._2) getValue
  where
    getValue = \case
        BoolRange -> do
            val <- [False, True]
            return (SomeExpr (Literal val TyBool) TyBool)
        IntRange (lower, upper) -> do
            val <- fromIntegral <$> [lower .. upper]
            return (SomeExpr (Literal val TyInt) TyInt)


-- | Substitute a 'Variable' with the given expression everywhere.
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


type Updates = [TElem (TUpdate Elem)]


-- | Get the 'Range's of all variables appearing inside an 'Index' operator.
getIndexRanges
    :: ( MonadReader r m
       , HasSymbolTable r
       , HasConstants r
       , HasRangeTable r
       , HasExprs a
       )
    => (a -> Updates)
    -> Maybe Component
    -> a
    -> m [(Variable, Range)]
getIndexRanges getUpdates mComp cmd = do
    vars <- Set.unions <$> traverse (variables mComp) (indexExprs getUpdates cmd)
    getVariableRanges (toList vars)


-- | Retrieve all 'Expr's that appear as an index inside an 'Index' operator.
indexExprs :: HasExprs a => (a -> Updates) -> a -> [Some Expr]
indexExprs getUpdates cmd =
    mapMaybe indexExpr (universeExprs cmd) ++
    concatMap assignmentIndexExprs assignments
  where
    assignments =
        concatMap (fmap getElem . updAssignments . getElem) (getUpdates cmd)

    indexExpr (Some e) = case e of
        Index _ _ (Loc idx _) -> Just (Some idx)
        _                     -> Nothing

    assignmentIndexExprs (Assignment _ idxs _) = fmap fromLSomeExpr idxs

    fromLSomeExpr (Loc (SomeExpr e _) _) = Some e


-- | Get the 'Range' for each variable in the given list.
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


-- | Get the set of 'TypedVariable's contained in an 'Expr'.
variables ::
       (MonadReader r m, HasSymbolTable r, HasConstants r)
    => Maybe Component
    -> Some Expr
    -> m (Set TypedVariable)
variables mComp (Some e) =
    fmap (Set.fromList . catMaybes) (traverse variable (universeExpr e))
  where
    variable (Some e') = do
        symTable <- view symbolTable
        consts   <- view constants
        return $ case e' of
            Identifier name ty
                | Just comp <- mComp
                , isLocalSymbol symTable (view compTypeName comp) name ->
                    Just (TypedVariable
                        (Variable name (LocalVar
                            (view compTypeName comp)
                            (componentName (view compName comp))))
                        (Some ty))
                | isGlobalSymbol symTable name &&
                  not (isConstant consts name) ->
                    Just (TypedVariable (Variable name GlobalVar) (Some ty))
            Member (Identifier cName (TyComponent (toList -> [tyName']))) name ty
                | isLocalSymbol symTable tyName' name ->
                    Just (TypedVariable
                        (Variable name (LocalVar tyName' cName))
                        (Some ty))
            _ -> Nothing
