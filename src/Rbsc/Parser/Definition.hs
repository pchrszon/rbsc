{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}


-- | Top-level definitions.
module Rbsc.Parser.Definition
    ( ErrorOrDef

    , Definition(..)
    , toModel
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict

import           Data.Foldable
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Traversable

import Text.Megaparsec


import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..))

import Rbsc.Syntax.Untyped


-- | A parse error or a definition.
type ErrorOrDef = Either (ParseError Char Dec) Definition


-- | Top-level definitions of a model.
data Definition
    = DefConstant UConstant
    | DefFunction UFunction
    | DefGlobal UGlobal
    | DefNaturalType NaturalTypeDef
    | DefRoleType RoleTypeDef
    | DefCompartmentType UCompartmentTypeDef
    | DefSystem [Loc Expr]
    | DefImplementation UImplementation
    | DefModule UModule
    deriving (Show)

makePrisms ''Definition


-- | Extract a 'Model' from a list of definitions.
toModel :: MonadError [Error] m => [Definition] -> m Model
toModel defs = do
    impls <- getImplementations defs
    return Model
        { modelConstants        = def _DefConstant
        , modelFunctions        = def _DefFunction
        , modelGlobals          = def _DefGlobal
        , modelNaturalTypes     = def _DefNaturalType
        , modelRoleTypes        = def _DefRoleType
        , modelCompartmentTypes = def _DefCompartmentType
        , modelSystem           = concat (def _DefSystem)
        , modelImpls            = impls
        }
  where
    def p = toListOf (traverse.p) defs


getImplementations ::
       MonadError [Error] m => [Definition] -> m (Map TypeName [UModuleBody])
getImplementations defs = do
    mods <- getModules defs
    let impls = toListOf (traverse._DefImplementation) defs
    Map.unions <$> traverse (fromImpl mods) impls
  where
    fromImpl mods (Implementation (Loc tyName _) body) =
        fmap (Map.singleton tyName) $ case body of
            ImplSingle b -> return [b]
            ImplModules ms ->
                for (toList ms) $ \(Loc name rgn) ->
                    case Map.lookup name mods of
                        Just b  -> return (unLoc b)
                        Nothing -> throwError [Error rgn UndefinedModule]


getModules ::
       MonadError [Error] m => [Definition] -> m (Map Name (Loc UModuleBody))
getModules defs = flip execStateT Map.empty $
    forOf_ (traverse._DefModule) defs $ \(Module (Loc name rgn) body) ->
        use (at name) >>= \case
            Just (Loc _ first) -> throwError [Error rgn (DuplicateModule first)]
            Nothing -> at name .= Just (Loc body rgn)
