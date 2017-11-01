{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


-- | The type system of the modeling language and its embedding into the
-- Haskell type system.
module Rbsc.Data.Type
    ( Type(..)
    , AType(..)
    , typeEq

    , dictShow
    , dictEq

    , checkNum
    , numTypes

    , checkOrd
    , ordTypes

    , (:~:)(..)
    , Dict(..)
    ) where


import Data.Constraint           (Dict (..))
import Data.Text.Prettyprint.Doc
import Data.Type.Equality        ((:~:) (..))

import Rbsc.Data.Component
import Rbsc.Data.Name


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
    AType s == AType t =
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


-- | The 'Show' @t@ type class dictionary for the 'Type' @t@.
dictShow :: Type t -> Dict (Show t)
dictShow = \case
    TyBool        -> Dict
    TyInt         -> Dict
    TyDouble      -> Dict
    TyArray ty    -> case dictShow ty of Dict -> Dict
    TyComponent _ -> Dict


-- | The 'Eq' @t@ type class dictionary for the 'Type' @t@.
dictEq :: Type t -> Dict (Eq t)
dictEq = \case
    TyBool        -> Dict
    TyInt         -> Dict
    TyDouble      -> Dict
    TyArray ty    -> case dictEq ty of Dict -> Dict
    TyComponent _ -> Dict


-- | Check if for a given 'Type' @t@ whether @t@ is an instance of 'Num'.
checkNum :: Type t -> Maybe (Dict (Num t))
checkNum = \case
    TyInt    -> return Dict
    TyDouble -> return Dict
    _        -> Nothing


-- | List of number types.
numTypes :: [AType]
numTypes = [AType TyInt, AType TyDouble]


-- | Check if for a given 'Type' @t@ whether @t@ is an instance of 'Ord'.
checkOrd :: Type t -> Maybe (Dict (Ord t))
checkOrd = \case
    TyInt      -> return Dict
    TyDouble   -> return Dict
    TyArray ty -> do
        Dict <- checkOrd ty
        return Dict
    _ -> Nothing


-- | List of primitive types that can be compared.
ordTypes :: [AType]
ordTypes = [AType TyInt, AType TyDouble]
