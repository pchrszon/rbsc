{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}


-- | Removal of variables used as indices.
--
-- If a variable is used inside index brackets, e.g., @arr[x]@, where @x@
-- is a variable, the index must be made constant. This is accomplished by
-- replicating the command for every possible value of @x@. Then, the guard
-- of the command is extended by @x = value@.
module Rbsc.Translator.Indices
    ( removeVariableIndices
    ) where


import Control.Monad.Reader


import Rbsc.Data.Component
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))

import Rbsc.Translator.Indices.Internal


-- | Remove all variables appearing inside indexing brackets by replacing
-- them with all possible values.
removeVariableIndices ::
       (MonadEval r m, HasSymbolTable r, HasRangeTable r)
    => Component
    -> TModuleBody Elem
    -> m (TModuleBody Elem)
removeVariableIndices comp (ModuleBody vars cmds) =
    ModuleBody vars . concat <$>
        traverse (fmap (fmap Elem) . removeInCommand comp . getElem) cmds


removeInCommand ::
       (MonadReader r m, HasConstants r, HasSymbolTable r, HasRangeTable r)
    => Component
    -> TCommand Elem
    -> m [TCommand Elem]
removeInCommand comp cmd = do
    ranges <- getIndexRanges comp cmd
    return (fmap remove (variableValues ranges))
  where
    remove vals =
        let cmd' = foldr substituteVariable cmd vals
            g = (case cmdGuard cmd' of
                    Loc (SomeExpr e TyBool) _ -> e
                    _ -> error "removeVariableIndices: type error") :: Expr Bool
            g' = LogicOp And (valueGuard vals) g
        in cmd' { cmdGuard = SomeExpr g' TyBool `withLocOf` cmdGuard cmd }
