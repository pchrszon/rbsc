{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Abstract syntax of variable declarations
module Language.Prism.Declaration
    ( Declaration(..)
    , DeclarationType(..)
    ) where


import Data.Text.Prettyprint.Doc

import Language.Prism.Expression


-- | A variable declaration.
data Declaration = Declaration
    { declIdent :: !Ident
    , declType  :: DeclarationType
    , declInit  :: Maybe Expr
    } deriving (Show)

instance Pretty Declaration where
    pretty Declaration{..} =
        pretty declIdent <+> colon <+> pretty declType <>
        maybe mempty ((" init" <+>) . pretty) declInit <> semi


-- | Represents the type of a variable.
data DeclarationType
    = DeclTypeBool
    | DeclTypeInt !Expr !Expr
    | DeclTypeIntUnbounded
    deriving (Show)

instance Pretty DeclarationType where
    pretty = \case
        DeclTypeBool         -> "bool"
        DeclTypeInt low high -> brackets (pretty low <> ".." <> pretty high)
        DeclTypeIntUnbounded -> "int"
