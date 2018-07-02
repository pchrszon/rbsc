{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


-- | The type system of the modeling language and its embedding into the
-- Haskell type system.
module Rbsc.Data.Type
    ( -- * Representation of types
      Type(..)
    , Fn(..)
    , (-->)
    , arrayLength

      -- * Symbol table
    , SymbolTable
    , HasSymbolTable(..)

      -- * Range table
    , RangeTable
    , HasRangeTable(..)

      -- * Type equality
    , (:~:)(..)
    , typeEq

      -- * Type class dictionaries
    , Dict(..)

    , dictShow

    , checkEq

    , checkNum
    , numTypes

    , checkOrd
    , ordTypes

    , module Rbsc.Data.Name
    ) where


import Control.Lens

import Data.Constraint           (Dict (..))
import Data.Foldable             (toList)
import Data.Map.Strict           (Map)
import Data.Set                  (Set)
import Data.Text.Prettyprint.Doc
import Data.Type.Equality        ((:~:) (..))


import Rbsc.Data.Action
import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.Name
import Rbsc.Data.Scope
import Rbsc.Data.Some


-- | Value-level representation of types.
data Type t where
    TyBool      :: Type Bool
    TyInt       :: Type Integer
    TyDouble    :: Type Double
    TyAction    :: Type Action
    TyComponent :: Set TypeName -> Type Component
    TyArray     :: (Int, Int) -> Type t -> Type (Array t)
    TyFunc      :: Type a -> Type b -> Type (Fn (a -> b))

deriving instance Eq (Type t)
deriving instance Show (Type t)

instance Pretty (Type t) where
    pretty = \case
        TyBool   -> "bool"
        TyInt    -> "int"
        TyDouble -> "double"
        TyAction -> "action"
        TyComponent tySet ->
            braces (sep (punctuate comma (fmap pretty (toList tySet))))
        TyArray (idxStart, idxEnd) t ->
            "array" <+> brackets (pretty idxStart <> ".." <> pretty idxEnd) <+>
            "of" <+> pretty t
        TyFunc a b -> parens (pretty a <+> "->" <+> pretty b)


-- | Wrapper for a function.
--
-- This wrapper provides dummy instances for 'Show' which (for obvious reasons)
-- does not exist for the type @a -> b@.
newtype Fn a = Fn { getFn :: a }

instance Show (Fn a) where
    show _ = "<function>"


infixr 9 -->
-- | Infix operator for 'TyFunc'.
(-->) :: Type a -> Type b -> Type (Fn (a -> b))
(-->) = TyFunc


-- | @arrayLength (indexStart, indexEnd)@ computes the length of an array
-- with the given first and last element indices (@indexEnd@ is inclusive).
arrayLength :: (Int, Int) -> Int
arrayLength (idxStart, idxEnd) = idxEnd - idxStart + 1


deriving instance Show (Some Type)

instance Eq (Some Type) where
    Some s == Some t =
        case typeEq s t of
            Just Refl -> True
            Nothing   -> False

instance Pretty (Some Type) where
    pretty (Some ty) = pretty ty


-- | The symbol table holds the type of each identifier in the model
-- source.
type SymbolTable = Map ScopedName (Some Type)


class HasSymbolTable a where
    symbolTable :: Lens' a SymbolTable


-- | The range table holds the range of each integer variable in the model.
-- If a variable has an array type, where the base type is integer, then
-- this variable is also present in the range table. (It is sufficient to
-- store the range for the array variable, since all elements of the array
-- have the same range.)
type RangeTable = Map ScopedName (Int, Int)


class HasRangeTable a where
    rangeTable :: Lens' a RangeTable


-- | Check the equality of 'Type's.
--
-- Array types are considered equal if they have the same length,
-- regardless of their respective start index.
--
-- The user-defined type of components is not checked for equality.
typeEq :: Type s -> Type t -> Maybe (s :~: t)
typeEq TyBool      TyBool   = Just Refl
typeEq TyInt       TyInt    = Just Refl
typeEq TyDouble    TyDouble = Just Refl
typeEq TyAction    TyAction = Just Refl
typeEq (TyComponent _) (TyComponent _) = Just Refl
typeEq (TyArray sIndices s) (TyArray tIndices t) = do
    Refl <- typeEq s t
    if arrayLength sIndices == arrayLength tIndices
        then Just Refl
        else Nothing
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
    TyAction      -> Dict
    TyComponent _ -> Dict
    TyArray _ ty  -> case dictShow ty of Dict -> Dict
    TyFunc _ _    -> Dict


-- | Check if a given 'Type' @t@ is an instance of 'Eq'.
checkEq :: Type t -> Maybe (Dict (Eq t))
checkEq = \case
    TyBool        -> return Dict
    TyInt         -> return Dict
    TyDouble      -> return Dict
    TyComponent _ -> return Dict
    TyArray _ ty  -> do
        Dict <- checkEq ty
        return Dict
    _ -> Nothing


-- | Check if a given 'Type' @t@ is an instance of 'Num'.
checkNum :: Type t -> Maybe (Dict (Num t))
checkNum = \case
    TyInt    -> return Dict
    TyDouble -> return Dict
    _        -> Nothing


-- | List of number types.
numTypes :: [Some Type]
numTypes = [Some TyInt, Some TyDouble]


-- | Check if a given 'Type' @t@ is an instance of 'Ord'.
checkOrd :: Type t -> Maybe (Dict (Ord t))
checkOrd = \case
    TyInt        -> return Dict
    TyDouble     -> return Dict
    TyArray _ ty -> do
        Dict <- checkOrd ty
        return Dict
    _ -> Nothing


-- | List of primitive types that can be compared.
ordTypes :: [Some Type]
ordTypes = [Some TyInt, Some TyDouble]
