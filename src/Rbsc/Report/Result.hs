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

    , fromEither
    , fromEither'
    , toEither
    , toEither'

    , throw
    , throwMany
    , warn

    , Error(..)
    , LocErrorDesc(..)
    , NoLocErrorDesc(..)
    , Warning(..)
    ) where


import Control.Lens              hiding (Empty)
import Control.Monad.Error.Class (MonadError (..))

import GHC.Exts (IsList (..))


import           Rbsc.Data.Bag (Bag)
import qualified Rbsc.Data.Bag as Bag

import Rbsc.Report.Error
import Rbsc.Report.Warning


-- | A @Result@ contains a 'Bag' of 'Warning's and either the result of
-- a computation or a @Bag@ of 'Error's.
data Result a
    = Result (Bag Warning) (Either (Bag Error) a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative Result where
    pure = Result mempty . Right

    Result wl l <*> Result wr r =
        Result (wl `mappend` wr) $ case (l, r) of
            (Right f , Right x)  -> Right (f x)
            (Left esl, Left esr) -> Left (esl <> esr)
            (Left es , _)        -> Left es
            (_       , Left es)  -> Left es

instance Monad Result where
    Result ws r >>= k = case r of
        Left es -> Result ws (Left es)
        Right x ->
            let Result ws' r' = k x
            in Result (ws <> ws') r'

instance MonadError Error Result where
    throwError = Result mempty . Left . Bag.singleton
    catchError (Result ws (Right x)) _ = Result ws (Right x)
    catchError (Result ws (Left es)) h = case toList es of
        []  -> error "Result.catchError: empty list"
        [e] ->
            let Result ws' r' = h e
            in Result (ws <> ws') r'
        (e:es') ->
            let Result ws' _ = h e
            in Result (ws <> ws') (Left (Bag.fromList es'))


-- | Throw many 'Error's.
throwMany :: [Error] -> Result a
throwMany = Result mempty . Left . Bag.fromList


-- | Emit a 'Warning'.
warn :: Warning -> Result ()
warn w = Result (Bag.singleton w) (Right ())


-- | Convert an 'Either' value into a 'Result' value.
fromEither :: Either [Error] a -> Result a
fromEither = \case
    Right x -> Result mempty (Right x)
    Left es -> Result mempty (Left (Bag.fromList es))


-- | Convert an 'Either', possibly containing only a single 'Error', into
-- a 'Result'.
fromEither' :: Either Error a -> Result a
fromEither' = fromEither . over _Left (: [])


-- | Convert a 'Result' into an 'Either' value, discarding all 'Warning's.
toEither :: Result a -> Either [Error] a
toEither = fst . toEither'


-- | Convert a 'Result' into a list of 'Warning's and an 'Either' value.
toEither' :: Result a -> (Either [Error] a, [Warning])
toEither' (Result ws r) = (over _Left toList r, toList ws)
