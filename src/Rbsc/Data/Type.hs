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
    , (-->)

    , SomeType(..)

    , typeEq

    , dictShow

    , checkEq

    , checkNum
    , numTypes

    , checkOrd
    , ordTypes

    , (:~:)(..)
    , Dict(..)

    , module Rbsc.Data.Name
    ) where


import Control.Applicative

import Data.Constraint           (Dict (..))
import Data.Text.Prettyprint.Doc
import Data.Type.Equality        ((:~:) (..))


import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.Function
import Rbsc.Data.Name


-- | Value-level representation of types.
data Type t where
    TyBool      :: Type Bool
    TyInt       :: Type Integer
    TyDouble    :: Type Double
    TyComponent :: Maybe TypeName -> Type Component
    TyArray     :: Type t -> Maybe Int -> Type (Array t)
    TyFunc      :: Type a -> Type b -> Type (Fn (a -> b))

deriving instance Eq (Type t)
deriving instance Show (Type t)

instance Pretty (Type t) where
    pretty = \case
        TyBool   -> "bool"
        TyInt    -> "int"
        TyDouble -> "double"
        TyComponent tyName -> maybe "component" pretty tyName
        TyArray t mLen ->
            brackets (pretty t <> maybe emptyDoc (("; " <>) . pretty) mLen)
        TyFunc a b -> parens (pretty a <+> "->" <+> pretty b)


infixr 9 -->
-- | Infix operator for 'TyFunc'.
(-->) :: Type a -> Type b -> Type (Fn (a -> b))
(-->) = TyFunc


-- | Existentially quantified 'Type'.
data SomeType where
    SomeType :: Type t -> SomeType

instance Eq SomeType where
    SomeType s == SomeType t =
        case typeEq s t of
            Just Refl -> True
            Nothing   -> False

deriving instance Show SomeType

instance Pretty SomeType where
    pretty (SomeType ty) = pretty ty


-- | Check the equality of 'Type's.
--
-- If array types are checked for equality and both array types have
-- a known size, the size is checked for equality as well. Otherwise, the
-- array size is ignored.
--
-- The user-defined type of components is not checked for equality.
typeEq :: Type s -> Type t -> Maybe (s :~: t)
typeEq TyBool      TyBool   = Just Refl
typeEq TyInt       TyInt    = Just Refl
typeEq TyDouble    TyDouble = Just Refl
typeEq (TyComponent _) (TyComponent _) = Just Refl
typeEq (TyArray s sLen) (TyArray t tLen) = do
    Refl <- typeEq s t
    case liftA2 (==) sLen tLen of
        Just False -> Nothing
        _ ->          Just Refl
typeEq (TyFunc a b) (TyFunc c d) = do
    Refl <- typeEq a c
    Refl <- typeEq b d
    Just Refl
typeEq _ _ = Nothing


-- | The 'Show' @t@ type class dictionary for the 'Type' @t@.
dictShow :: Type t -> Dict (Show t)
dictShow = \case
    TyBool        -> Dict
    TyInt         -> Dict
    TyDouble      -> Dict
    TyComponent _ -> Dict
    TyArray ty _  -> case dictShow ty of Dict -> Dict
    TyFunc _ _    -> Dict


-- | Check if for a given 'Type' @t@ whether @t@ is an instance of 'Eq'.
checkEq :: Type t -> Maybe (Dict (Eq t))
checkEq = \case
    TyBool        -> return Dict
    TyInt         -> return Dict
    TyDouble      -> return Dict
    TyComponent _ -> return Dict
    TyArray ty _  -> do
        Dict <- checkEq ty
        return Dict
    _ -> Nothing


-- | Check if for a given 'Type' @t@ whether @t@ is an instance of 'Num'.
checkNum :: Type t -> Maybe (Dict (Num t))
checkNum = \case
    TyInt    -> return Dict
    TyDouble -> return Dict
    _        -> Nothing


-- | List of number types.
numTypes :: [SomeType]
numTypes = [SomeType TyInt, SomeType TyDouble]


-- | Check if for a given 'Type' @t@ whether @t@ is an instance of 'Ord'.
checkOrd :: Type t -> Maybe (Dict (Ord t))
checkOrd = \case
    TyInt        -> return Dict
    TyDouble     -> return Dict
    TyArray ty _ -> do
        Dict <- checkOrd ty
        return Dict
    _ -> Nothing


-- | List of primitive types that can be compared.
ordTypes :: [SomeType]
ordTypes = [SomeType TyInt, SomeType TyDouble]
