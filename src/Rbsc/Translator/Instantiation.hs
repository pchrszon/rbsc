{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


-- | Instantiation of component implementations.
module Rbsc.Translator.Instantiation
    ( instantiateComponent
    ) where


import Control.Lens

import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Traversable


import Rbsc.Data.Component
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type(..))


-- | Instantiate all 'ModuleBody's for the given 'Component'.
instantiateComponent ::
       Constants
    -> RecursionDepth
    -> Model
    -> Component
    -> Either Error [TModuleBody Elem]
instantiateComponent consts depth m comp =
    traverse (instantiateModuleBody consts depth comp) bodies
  where
    bodies = fromMaybe [] (Map.lookup (view compTypeName comp) (modelImpls m))


instantiateModuleBody ::
       Constants
    -> RecursionDepth
    -> Component
    -> TModuleBody ElemMulti
    -> Either Error (TModuleBody Elem)
instantiateModuleBody consts depth comp body =
    unrollModuleBody consts depth (substituteSelf comp body)


unrollModuleBody ::
       Constants
    -> RecursionDepth
    -> TModuleBody ElemMulti
    -> Either Error (TModuleBody Elem)
unrollModuleBody consts depth ModuleBody{..} = do
    cmds <- unrollElemMultis consts depth bodyCommands
    cmds' <- for cmds $ \(Elem cmd) -> Elem <$> unrollCommand consts depth cmd
    return (ModuleBody bodyVars cmds')


unrollCommand ::
       Constants
    -> RecursionDepth
    -> TCommand ElemMulti
    -> Either Error (TCommand Elem)
unrollCommand consts depth Command{..} = do
    upds <- unrollElemMultis consts depth cmdUpdates
    upds' <- for upds $ \(Elem upd) -> Elem <$> unrollUpdate consts depth upd
    return (Command cmdAction cmdGuard upds')


unrollUpdate ::
       Constants
    -> RecursionDepth
    -> TUpdate ElemMulti
    -> Either Error (TUpdate Elem)
unrollUpdate consts depth Update{..} =
    Update updProb <$> unrollElemMultis consts depth updAssignments


unrollElemMultis ::
       HasExprs a
    => Constants
    -> RecursionDepth
    -> [TElemMulti a]
    -> Either Error [TElem a]
unrollElemMultis consts depth elems =
    concat <$> traverse (unrollElemMulti consts depth) elems


unrollElemMulti ::
       HasExprs a
    => Constants
    -> RecursionDepth
    -> TElemMulti a
    -> Either Error [TElem a]
unrollElemMulti consts depth = \case
    ElemSingle x   -> return [Elem x]
    ElemLoop l     -> unrollLoop l
    ElemIf g elems -> case g of
        Loc (SomeExpr g' TyBool) rgn -> do
            b <- eval consts depth (Loc g' rgn)
            if b
                then unrollElemMultis consts depth elems
                else return []
        _ -> error "unrollElemMulti: type error"
  where
    unrollLoop Loop{..} = do
        es <-
            case loopType of
                QdTypeComponent tySet -> return (componentConsts tySet consts)
                QdTypeInt (lower, upper) -> do
                    l <- eval consts depth lower
                    u <- eval consts depth upper
                    return (fmap (\i -> SomeExpr (Literal i) TyInt) [l .. u])
        let bodies' = concatMap (\e -> fmap (instantiateExprs e) loopBody) es
        unrollElemMultis consts depth bodies'


substituteSelf :: Component -> TModuleBody ElemMulti -> TModuleBody ElemMulti
substituteSelf comp ModuleBody{..} =
    ModuleBody bodyVars (fmap (transformExprs subst) bodyCommands)
  where
    subst :: Expr t -> Expr t
    subst e = case e of
        Self -> Literal comp
        _    -> e
