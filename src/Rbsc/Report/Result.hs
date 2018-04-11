{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}


-- | This module provides the 'Result' monad.
module Rbsc.Report.Result
    ( Result
    , Result'
    , Errors
    , fromEither
    , fromEither'
    , toEither
    , toEither'
    , throwOne
    , throwMany
    , warn

    , Error(..)
    , ErrorDesc(..)
    , Warning(..)
    ) where


import Control.Lens              hiding (Empty)
import Control.Monad.Error.Class (MonadError (..))

import Data.Semigroup

import GHC.Exts (IsList (..))


import           Rbsc.Data.Bag (Bag)
import qualified Rbsc.Data.Bag as Bag

import Rbsc.Report.Error
import Rbsc.Report.Warning


-- | A @Result@ contains a 'Bag' of 'Warning's and either the result of
-- a computation or errors.
data Result e a
    = Result (Bag Warning) (Either e a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Semigroup e => Applicative (Result e) where
    pure = Result mempty . Right

    Result wl l <*> Result wr r =
        Result (wl `mappend` wr) $ case (l, r) of
            (Right f , Right x)  -> Right (f x)
            (Left esl, Left esr) -> Left (esl <> esr)
            (Left es , _)        -> Left es
            (_       , Left es)  -> Left es

instance Semigroup e => Monad (Result e) where
    Result ws r >>= k = case r of
        Left es -> Result ws (Left es)
        Right x ->
            let Result ws' r' = k x
            in Result (ws <> ws') r'

instance Semigroup e => MonadError e (Result e) where
    throwError = Result mempty . Left
    catchError (Result ws (Right x)) _ = Result ws (Right x)
    catchError (Result ws (Left es)) h =
        let Result ws' r' = h es
        in Result (ws <> ws') r'


-- | The 'Result' type where the error type is specialized to 'Errors'.
type Result' a = Result Errors a


-- | A 'Bag' of 'Error's.
type Errors = Bag Error


-- | Throw a single 'Error'.
throwOne :: MonadError Errors m => Error -> m a
throwOne = throwError . Bag.singleton


-- | Throw many 'Error's.
throwMany :: MonadError Errors m => [Error] -> m a
throwMany = throwError . Bag.fromList


-- | Emit a 'Warning'.
warn :: Warning -> Result e ()
warn w = Result (Bag.singleton w) (Right ())


-- | Convert an 'Either' value into a 'Result' value.
fromEither :: Either [Error] a -> Result (Bag Error) a
fromEither = \case
    Right x -> Result mempty (Right x)
    Left es -> Result mempty (Left (Bag.fromList es))


-- | Convert an 'Either', possibly containing only a single 'Error', into
-- a 'Result'.
fromEither' :: Either Error a -> Result (Bag Error) a
fromEither' = fromEither . over _Left (: [])


-- | Convert a 'Result' into an 'Either' value, discarding all 'Warning's.
toEither :: Result (Bag Error) a -> Either [Error] a
toEither = fst . toEither'


-- | Convert a 'Result' into a list of 'Warning's and an 'Either' value.
toEither' :: Result (Bag Error) a -> (Either [Error] a, [Warning])
toEither' (Result ws r) = (over _Left toList r, toList ws)
