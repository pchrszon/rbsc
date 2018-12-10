{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


-- | Conversion of multi-actions to single actions.
module Rbsc.Translator.Convert
    ( convertToSingleActions
    , prepareConversion
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Language.Prism         as Prism
import qualified Language.Prism.Convert as Prism

import Data.Text (intercalate, isPrefixOf, pack)


import Rbsc.Data.Field

import Rbsc.Translator.Internal (overridePrefix)

import Rbsc.Util.NameGen


type Identifiers = Map [Prism.Action] Prism.Ident


type ConverterState =
    NameGen :&:
    Identifiers


idents :: Lens' ConverterState Identifiers
idents = field


type Converter a = State ConverterState a


convertToSingleActions :: Prism.ConversionInfo -> Prism.Model -> Prism.Model
convertToSingleActions ci =
    removeDesyncModule . runConverter . Prism.convertToSingleActions convert ci
  where
    removeDesyncModule model = model
        { Prism.modelModules =
            filter (not . isDesyncModule) (Prism.modelModules model)
        }

    isDesyncModule = ("Desync" ==) . Prism.modIdent


prepareConversion :: Prism.Model -> Prism.ConversionInfo
prepareConversion = Prism.conversionInfo


runConverter :: Converter a -> a
runConverter m = evalState m initState
  where
    initState = mkNameGen id Set.empty :&: Map.empty


convert :: [Prism.Action] -> Converter Prism.Ident
convert acts = use (idents.at acts) >>= \case
    Just ident -> return ident
    Nothing -> do
        let ident = mkIdent acts
        ident' <- newNameFrom ident

        idents.at acts ?= ident'

        return ident'
  where
    mkIdent =
        intercalate "_" . fmap actionToIdent . filter (not . isOverrideAction)

    actionToIdent = \case
        Prism.Action act -> act
        Prism.Tau i      -> "tau" <> pack (show i)

    isOverrideAction = \case
        Prism.Action act | overridePrefix `isPrefixOf` act -> True
        _ -> False
