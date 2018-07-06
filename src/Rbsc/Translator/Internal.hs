{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Rbsc.Translator.Internal
    ( trnsQualified
    , addIndex
    , reduceLSomeExpr
    ) where


import Data.Semigroup
import Data.Text      (pack)


import qualified Language.Prism as Prism


import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))


trnsQualified :: Monad m => Qualified -> m Prism.Ident
trnsQualified = return . go
  where
    go = \case
        QlName name         -> name
        QlMember inner name -> go inner <> "_" <> name
        QlIndex inner idx   -> go inner <> "_" <> pack (show idx)


addIndex :: Int -> LSomeExpr -> LSomeExpr
addIndex i (Loc (SomeExpr e (TyArray _ innerTy)) rgn) =
    case dictShow innerTy of
        Dict ->
            let e' = Index e (Loc (Literal (fromIntegral i) TyInt) rgn)
            in Loc (SomeExpr e' innerTy) rgn
addIndex _ _ = error "trnsVarDecl: not an array"


reduceLSomeExpr :: MonadEval r m => LSomeExpr -> m LSomeExpr
reduceLSomeExpr (Loc (SomeExpr e ty) rgn) = do
    Loc e' _ <- reduce (Loc e rgn)
    return (Loc (SomeExpr e' ty) rgn)
