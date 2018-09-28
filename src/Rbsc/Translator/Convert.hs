{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Conversion of multi-actions to single actions.
module Rbsc.Translator.Convert
    ( convertToSingleActions
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import qualified Language.Prism         as Prism
import qualified Language.Prism.Convert as Prism

import Data.Text (intercalate, isPrefixOf, pack)


import Rbsc.Util.NameGen


data ConverterState = ConverterState
    { _csNameGen :: NameGen
    , _csIdents  :: !(Map [Prism.Action] Prism.Ident)
    }

makeLenses ''ConverterState

instance HasNameGen ConverterState where
    nameGen = csNameGen


type Converter a = State ConverterState a


convertToSingleActions :: Prism.Model -> Prism.Model
convertToSingleActions =
    removeDesyncModule . runConverter . Prism.convertToSingleActions convert
  where
    removeDesyncModule model = model
        { Prism.modelModules = filter (not . isDesyncModule) (Prism.modelModules model)
        }

    isDesyncModule = ("Desync" ==) . Prism.modIdent


runConverter :: Converter a -> a
runConverter m = evalState m initState
  where
    initState = ConverterState (mkNameGen id Set.empty) Map.empty


convert :: [Prism.Action] -> Converter Prism.Ident
convert acts = use (csIdents.at acts) >>= \case
    Just ident -> return ident
    Nothing -> do
        let ident = mkIdent acts
        ident' <- newNameFrom ident

        csIdents.at acts ?= ident'

        return ident'
  where
    mkIdent = intercalate "_" . fmap actionToIdent . filter (not . isOverrideAction)

    actionToIdent = \case
        Prism.Action act -> act
        Prism.Tau i      -> "tau" <> pack (show i)

    isOverrideAction = \case
        Prism.Action act | "ovr" `isPrefixOf` act -> True
        _ -> False
