{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}


-- | Type-directed field accessors for arbitrarily nested pairs.
module Rbsc.Data.Field
    ( (:&:)(..)
    , Has
    , field
    ) where


import Control.Lens hiding (Contains (..))


infixr 2 :&:


-- | A strict pair.
data (a :&: b) = !a :&: !b deriving (Eq, Ord, Show)

instance (Semigroup a, Semigroup b) => Semigroup (a :&: b) where
    (x1 :&: y1) <> (x2 :&: y2) = (x1 <> x2) :&: (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (a :&: b) where
    mempty = mempty :&: mempty


-- | @Has a s@ states that a value of type @s@ contains a value of type @a@.
type Has a s = Contains (Elem a s) a s


-- | A 'Lens' providing access to the field with type @a@.
field :: forall a s. Has a s => Lens' s a
field = field' @(Elem a s)


class Contains (res :: Res) a s where
    field' :: Lens' s a

instance Contains ('Found 'Here) a a where
    field' = id

instance Contains ('Found p) a l => Contains ('Found ('L p)) a (l :&: r) where
    field' f (l :&: r) = (:&: r) <$> field' @('Found p) f l

instance Contains ('Found p) a r => Contains ('Found ('R p)) a (l :&: r) where
    field' f (l :&: r) = (l :&:) <$> field' @('Found p) f r


-- | Type-level witness for finding a field.
data Pos = Here | L Pos | R Pos


-- | Type-level representation of a search result.
data Res = Found Pos | NotFound | Ambiguous


type family Elem a s :: Res where
    Elem x x         = 'Found 'Here
    Elem x (l :&: r) = Choose (Elem x l) (Elem x r)
    Elem x p         = 'NotFound


type family Choose (l :: Res) (r :: Res) :: Res where
    Choose ('Found l) ('Found r) = 'Ambiguous
    Choose 'Ambiguous r          = 'Ambiguous
    Choose l          'Ambiguous = 'Ambiguous
    Choose ('Found l) r          = 'Found ('L l)
    Choose l          ('Found r) = 'Found ('R r)
    Choose l          r          = 'NotFound
