{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}


-- | Translation of expressions.
module Rbsc.Translator.Expr
    ( trnsLSomeExpr
    ) where


import Control.Lens
import Control.Monad.Reader

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict    as Map
import           Data.Text          (pack)
import           Data.Traversable

import qualified Language.Prism as Prism


import Rbsc.Data.Action
import Rbsc.Data.Array     (Array)
import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Some
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region
import Rbsc.Report.Result

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Internal


trnsLSomeExpr
    :: Maybe (TypeName, ComponentName) -> LSomeExpr -> Translator Prism.Expr
trnsLSomeExpr mComp (Loc (SomeExpr e _) rgn) = trnsExpr mComp rgn e


trnsExpr
    :: Maybe (TypeName, ComponentName)
    -> Region
    -> Expr t
    -> Translator Prism.Expr
trnsExpr mComp rgn = go
  where
    go :: Expr t -> Translator Prism.Expr
    go e = do
        symTable <- view symbolTable
        case e of
            Index _ (Just size) (Loc (Literal i TyInt) rgn')
                | i < 0 || i >= size -> throw rgn' (IndexOutOfBounds size i)

            (trnsIdent symTable -> Just qname) ->
                Prism.Ident <$> trnsQualified qname

            Literal b TyBool ->
                return (Prism.LitBool b)

            Literal i TyInt ->
                return (Prism.LitInt (fromIntegral i))

            Literal d TyDouble ->
                return (Prism.LitDouble d)

            Cast inner ->
                go inner

            IsPlayed _ ->
                throw rgn IllegalPlayingConstraint

            Not inner ->
                Prism.UnaryOp Prism.Not <$> go inner

            Negate inner ->
                Prism.UnaryOp Prism.Neg <$> go inner

            ArithOp aOp l r ->
                Prism.BinaryOp <$> go l <*> pure (trnsArithOp aOp) <*> go r

            Divide _ l r ->
                Prism.BinaryOp <$> go l <*> pure Prism.Divide <*> go r

            EqOp eOp ty l r ->
                trnsEq mComp rgn eOp ty l r

            RelOp rOp l r ->
                Prism.BinaryOp <$> go l <*> pure (trnsRelOp rOp) <*> go r

            LogicOp lOp l r ->
                Prism.BinaryOp <$> go l <*> pure (trnsLogicOp lOp) <*> go r

            -- Index with a static index is already resolved at this point,
            -- so 'idx' is not a Literal.
            -- 'inner' cannot be an Action, since all indexed actions with
            -- a dynamic index are already rejected by the module instantiation.
            Index (trnsIdent symTable -> Just qname) (Just size) (Loc idx rgn') -> do
                lift (lift (warn (DynamicArrayAccess rgn')))

                let indexeds    = fmap (QlIndex qname) [0 .. size - 2]
                    indexedLast = QlIndex qname (size - 1)
                    conds       = fmap (idx `equal`) [0 .. size - 2]

                indexeds' <-
                    traverse (fmap Prism.Ident . trnsQualified) indexeds
                indexedLast' <- Prism.Ident <$> trnsQualified indexedLast
                conds'       <- traverse go conds

                return (foldr
                    (uncurry Prism.Ite) indexedLast' (zip conds' indexeds'))

            Index _ _ (Loc _ rgn') ->
                throw rgn' NotConstant

            Apply f arg ->
                trnsApply f [Some arg]

            IfThenElse c t e' ->
                Prism.Ite <$> go c <*> go t <*> go e'

            Playable (Loc (Literal comp (TyComponent _)) _) mAct -> do
                rgs <- getRoleGuards comp mAct
                g <- combineRoleGuards rgs
                trnsExpr
                    (Just (view compTypeName comp, view compName comp))
                    rgn
                    (unLoc g)

            Playable (Loc _ rgn') _ -> throw rgn' NotConstant

            e' ->
                throw rgn (TranslationNotSupported (pack (show e')))

    trnsIdent :: SymbolTable -> Expr t -> Maybe Qualified
    trnsIdent symTable = \case
        Identifier name _ -> case mComp of
            Just (typeName, compName') | isLocalSymbol symTable typeName name ->
                Just (QlMember (QlName (trnsComponentName compName')) name)
            _ -> Just (QlName name)

        Member (Literal comp (TyComponent _)) name _ ->
            let name' = trnsComponentName (view compName comp)
            in Just (QlMember (QlName name') name)

        Index (trnsIdent symTable -> Just qname) _ (Loc (Literal i _) _) ->
            Just (QlIndex qname i)

        _ -> Nothing


    trnsApply :: Expr t -> [Some Expr] -> Translator Prism.Expr
    trnsApply f args = case f of
        LitFunction l ->
            Prism.Func (trnsFunction l) <$> traverse (\(Some e) -> go e) args
        Apply f' arg -> trnsApply f' (Some arg : args)
        _ -> throw rgn (TranslationNotSupported (pack (show f)))

    equal :: Expr Int -> Int -> Expr Bool
    equal e i = EqOp Eq TyInt e (Literal i TyInt)


trnsEq
    :: Maybe (TypeName, ComponentName)
    -> Region
    -> EqOp
    -> Type t
    -> Expr t
    -> Expr t
    -> Translator Prism.Expr
trnsEq mComp rgn eOp = go
  where
    go :: Type t -> Expr t -> Expr t -> Translator Prism.Expr
    go ty l r = case ty of
        TyArray size innerTy -> do
            es' <- for [0 .. size - 1] $ \i ->
                    go innerTy (idx innerTy l i) (idx innerTy r i)
            return (foldr1 conn es')
        _ -> do
            Loc l' _ <- reduce (Loc l rgn)
            Loc r' _ <- reduce (Loc r rgn)
            Prism.BinaryOp
                <$> trnsExpr mComp rgn l'
                <*> pure (trnsEqOp eOp)
                <*> trnsExpr mComp rgn r'

    conn l r = case eOp of
               Eq  -> Prism.BinaryOp l Prism.And r
               NEq -> Prism.BinaryOp l Prism.Or r

    idx :: Type t -> Expr (Array t) -> Int -> Expr t
    idx ty e i = case dictShow ty of
        Dict -> Index e Nothing (Loc (Literal (fromIntegral i) TyInt) rgn)


trnsFunction :: TypedFunction t -> Prism.Function
trnsFunction = \case
    MinInt    -> Prism.FuncMin
    MinDouble -> Prism.FuncMin
    MaxInt    -> Prism.FuncMax
    MaxDouble -> Prism.FuncMax
    Floor     -> Prism.FuncFloor
    Ceil      -> Prism.FuncCeil
    PowInt    -> Prism.FuncPow
    PowDouble -> Prism.FuncPow
    Mod       -> Prism.FuncMod
    Log       -> Prism.FuncLog


trnsArithOp :: ArithOp -> Prism.BinaryOp
trnsArithOp = \case
    Add -> Prism.Plus
    Sub -> Prism.Minus
    Mul -> Prism.Times


trnsEqOp :: EqOp -> Prism.BinaryOp
trnsEqOp = \case
    Eq  -> Prism.Eq
    NEq -> Prism.Neq


trnsRelOp :: RelOp -> Prism.BinaryOp
trnsRelOp = \case
    Lt  -> Prism.Lt
    Lte -> Prism.Le
    Gt  -> Prism.Gt
    Gte -> Prism.Ge


trnsLogicOp :: LogicOp -> Prism.BinaryOp
trnsLogicOp = \case
    And     -> Prism.And
    Or      -> Prism.Or
    Implies -> Prism.Implies


combineRoleGuards :: NonEmpty LSomeExpr -> Translator (Loc (Expr Bool))
combineRoleGuards rgs = case foldr1 combine rgs of
    Loc (SomeExpr g TyBool) rgn -> reduce (Loc g rgn)
    _                           -> error "combineRoleGuards: type error"
  where
    combine :: LSomeExpr -> LSomeExpr -> LSomeExpr
    combine (Loc (SomeExpr l TyBool) lRgn) (Loc (SomeExpr r TyBool) rRgn) =
        Loc (SomeExpr (LogicOp Or l r) TyBool) (lRgn <> rRgn)
    combine _ _ = error "combineRoleGuards: type error"


getRoleGuards
    :: Component -> Maybe (Loc (Expr Action)) -> Translator (NonEmpty LSomeExpr)
getRoleGuards comp mAct =
    view (roleGuards.at (view compName comp)) >>= \case
        Just actMap -> case mAct of
            Just (Loc (Literal act TyAction) _) ->
                return (Map.findWithDefault (false :| []) (Just act) actMap)
            Just (Loc _ rgn) -> throw rgn NotConstant
            Nothing -> case Map.elems actMap of
                [] -> return (false :| [])
                gs -> return (foldr1 (<>) gs)
        Nothing -> error $ "getRoleGuards: component " ++
            show (view compName comp) ++ " not in roleGuards"
  where
    false = noLoc (SomeExpr (Literal False TyBool) TyBool)
    noLoc x = Loc x (Region "" "" (Position 1 1) (Position 1 1))
