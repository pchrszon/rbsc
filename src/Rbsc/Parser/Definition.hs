{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
import           Data.Void

import Text.Megaparsec


import Rbsc.Report.Error
import Rbsc.Report.Region (Loc (..))

import Rbsc.Syntax.Untyped


-- | A parse error or a definition.
type ErrorOrDef = Either (ParseError Char Void) Definition


-- | Top-level definitions of a model.
data Definition
    = DefConstant UConstant
    | DefFunction UFunction
    | DefGlobal UVarDecl
    | DefLabel ULabel
    | DefNaturalType NaturalTypeDef
    | DefRoleType RoleTypeDef
    | DefCompartmentType UCompartmentTypeDef
    | DefSystem [Loc Expr]
    | DefImplementation UImplementation
    | DefModule UModule
    | DefCoordinator UCoordinator
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
        , modelLabels           = def _DefLabel
        , modelNaturalTypes     = def _DefNaturalType
        , modelRoleTypes        = def _DefRoleType
        , modelCompartmentTypes = def _DefCompartmentType
        , modelSystem           = concat (def _DefSystem)
        , modelImpls            = impls
        , modelCoordinators     = def _DefCoordinator
        }
  where
    def p = toListOf (traverse.p) defs


getImplementations ::
       MonadError [Error] m
    => [Definition]
    -> m (Map TypeName [UNamedModuleBody])
getImplementations defs = do
    mods <- getModules defs
    let impls = toListOf (traverse._DefImplementation) defs
    Map.unions <$> traverse (fromImpl mods) impls
  where
    fromImpl mods (Implementation (Loc tyName _) body) =
        fmap (Map.singleton tyName) $ case body of
            ImplSingle b -> return [NamedModuleBody "impl" b]
            ImplModules ms ->
                for (toList ms) $ \(Loc name rgn) ->
                    case Map.lookup name mods of
                        Just b  -> return (NamedModuleBody name (unLoc b))
                        Nothing -> throwError [locError rgn UndefinedModule]


getModules ::
       MonadError [Error] m => [Definition] -> m (Map Name (Loc UModuleBody))
getModules defs = flip execStateT Map.empty $
    forOf_ (traverse._DefModule) defs $ \(Module (Loc name rgn) body) ->
        use (at name) >>= \case
            Just (Loc _ first) -> throwError
                [locError rgn (DuplicateModule first)]
            Nothing -> at name ?= Loc body rgn
