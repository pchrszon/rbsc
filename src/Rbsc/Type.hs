{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


-- | The type system of the modeling language and its embedding into the
-- Haskell type system.
module Rbsc.Type
    ( Type(..)
    , AType(..)
    , typeEq
    , (:~:)(..)
    ) where


import Data.Text.Prettyprint.Doc
import Data.Type.Equality        ((:~:) (..))

import Rbsc.Component
import Rbsc.Name


-- | Value-level representation of types.
data Type t where
    TyBool      :: Type Bool
    TyInt       :: Type Integer
    TyDouble    :: Type Double
    TyArray     :: Type t -> Type [t]
    TyComponent :: Maybe TypeName -> Type Component

deriving instance Eq (Type t)
deriving instance Show (Type t)

instance Pretty (Type t) where
    pretty = \case
        TyBool             -> "bool"
        TyInt              -> "int"
        TyDouble           -> "double"
        TyArray t          -> brackets (pretty t)
        TyComponent tyName -> maybe "component" pretty tyName


-- | Existentially quantified 'Type'.
data AType where
    AType :: Type t -> AType

instance Eq AType where
    (AType s) == (AType t) =
        case typeEq s t of
            Just Refl -> True
            Nothing   -> False

deriving instance Show AType

instance Pretty AType where
    pretty (AType ty) = pretty ty


-- | Check the equality of 'Type's.
--
-- The user-defined type of components is not checked for equality.
typeEq :: Type s -> Type t -> Maybe (s :~: t)
typeEq TyBool      TyBool      = Just Refl
typeEq TyInt       TyInt       = Just Refl
typeEq TyDouble    TyDouble    = Just Refl
typeEq (TyArray s) (TyArray t) = do
    Refl <- typeEq s t
    pure Refl
typeEq (TyComponent _) (TyComponent _) = Just Refl
typeEq _ _ = Nothing
