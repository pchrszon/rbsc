{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Rbsc.Translator.Internal
    ( trnsQualified
    ) where


import Data.Semigroup
import Data.Text      (pack)


import qualified Language.Prism as Prism


import Rbsc.Data.Type


trnsQualified :: Monad m => Qualified -> m Prism.Ident
trnsQualified = return . go
  where
    go = \case
        QlName name         -> name
        QlMember inner name -> go inner <> "_" <> name
        QlIndex inner idx   -> go inner <> "_" <> pack (show idx)
