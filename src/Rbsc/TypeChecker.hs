{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}


-- | Type checker for 'Model's.
module Rbsc.TypeChecker
    ( SomeExpr(..)
    , getExpr

    , typeCheck
    , extract
    ) where


import Rbsc.Data.ComponentType
import Rbsc.Data.SymbolTable

import qualified Rbsc.Report.Error.Type as Type
import           Rbsc.Report.Region     (Loc (..))

import           Rbsc.Syntax.Expr.Typed   (SomeExpr (..))
import qualified Rbsc.Syntax.Expr.Untyped as U

import Rbsc.TypeChecker.Expr
import Rbsc.TypeChecker.Internal


-- | Type check an untyped expression and transform it into a typed
-- expression.
typeCheck ::
       ComponentTypes -> SymbolTable -> Loc U.Expr -> Either Type.Error SomeExpr
typeCheck types symTable e = runTypeChecker (tcExpr e) types symTable
