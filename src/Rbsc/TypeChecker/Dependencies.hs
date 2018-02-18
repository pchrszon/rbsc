{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | This module provides a dependency analyzer for 'IdentifierDef' defined
-- in the model.
module Rbsc.TypeChecker.Dependencies
    ( Dependency(..)
    , sortDefinitions
    ) where


import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict

import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe
import           Data.Set           (Set)
import qualified Data.Set           as Set


import qualified Rbsc.Report.Error.Syntax as Syntax
import           Rbsc.Report.Region

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker.Identifiers

import Rbsc.Util


-- | A @Dependency@ corresponds to an 'IdentifierDef'. Each @Dependency@
-- may itself be dependend on other dependencies.
data Dependency
    = DepConstant UConstant
    | DepFunctionSignature UFunction
    | DepFunction UFunction
    | DepComponent !Name !TypeName
    deriving (Eq, Ord, Show)


data AnalyzerInfo = AnalyzerInfo
    { _identifiers       :: Identifiers -- ^ the identifiers defined in the model
    , _parameters        :: Set Name -- ^ the set of parameters that are in scope
    , _functions         :: Map Name UFunction -- ^ the functions defined in the model
    , _currentDependency :: Maybe (Loc Dependency) -- ^ the dependency that is currently created
    , _inFunctionBody    :: !Bool -- ^ @True@ if the expression is inside a funcion body
    }

makeLenses ''AnalyzerInfo


-- | Sort all identifier definitions such that each definition only refers
-- to definitions that are before it in the list. If such an ordering of the
-- definitions cannot be given (because the definitions contain a cycle),
-- then a 'CyclicDefinition' error is returned.
sortDefinitions :: Identifiers -> Either Syntax.Error [Dependency]
sortDefinitions idents = do
    depGraph <- runAnalyzer idents (traverse_ insert (Map.elems idents))
    case topoSort (Map.keys depGraph) (lookupEdges depGraph) of
        Right deps    -> return (fmap unLoc deps)
        Left depCycle -> throwError (buildError depCycle)
  where
    lookupEdges depGraph dep =
        toList (Map.findWithDefault Set.empty dep depGraph)

    buildError (Loc dep rgn :| deps) =
        Syntax.CyclicDefinition (getConstructName dep) rgn (fmap getLoc deps)

    getConstructName = \case
        DepConstant _          -> "constant"
        DepFunction _          -> "function"
        DepFunctionSignature _ -> "function"
        DepComponent _ _       -> "component"


-- | A dependency graph.
--
-- Each key maps to a set of other definitions it depends on.
type DependencyGraph = Map LDependency (Set LDependency)


type LDependency = Loc Dependency


-- | Insert an 'IdentifierDef' into the dependency graph.
insert :: Loc IdentifierDef -> Analyzer ()
insert (Loc def rgn) = case def of
    DefConstant c            -> insertConstant c
    DefFunction f            -> insertFunction f
    DefComponent name tyName -> insertComponent name tyName rgn


insertConstant :: UConstant -> Analyzer ()
insertConstant c@(Constant (Loc _ rgn) sTy e) =
    newDependency (DepConstant c) rgn (dependOnIdentifiers idents)
  where
    idents = identsInExpr e `Set.union` identsInType sTy


insertFunction :: UFunction -> Analyzer ()
insertFunction f@(Function (Loc _ rgn) _ _ e) = do
    newDependency (DepFunctionSignature f) rgn $
        dependOnIdentifiers (identsInSignature f)
    newDependency (DepFunction f) rgn $ do
        dependOn (DepFunctionSignature f) rgn
        local ((inFunctionBody .~ True) . (parameters .~ parameterSet f)) $
            dependOnIdentifiers (identsInExpr e)


insertComponent :: Name -> TypeName -> Region -> Analyzer ()
insertComponent name tyName rgn =
    newDependency (DepComponent name tyName) rgn $
        return ()


-- | @dependOnIdentifiers idents@ states that the current definition (see
-- 'newDependency') depends on all the identifiers contained in @idents@.
dependOnIdentifiers :: Foldable t => t (Loc Name) -> Analyzer ()
dependOnIdentifiers = traverse_ dependOnIdentifier . toList


dependOnIdentifier :: Loc Name -> Analyzer ()
dependOnIdentifier (Loc name rgn) = do
    params <- view parameters

    -- if the identifier is bound by a parameter, we skip it, since it does
    -- not depend on any identifier definition
    unless (name `Set.member` params) $ view (identifiers.at name) >>= \case
        Just (Loc def _) -> do
            deps <- depsFromIdentifierDef def
            for_ deps $ \dep ->
                dependOn dep rgn
        Nothing  -> throwError (Syntax.UndefinedIdentifier rgn)


depsFromIdentifierDef :: IdentifierDef -> Analyzer [Dependency]
depsFromIdentifierDef = \case
    DefConstant c -> return [DepConstant c]
    DefFunction f -> do
        inBody <- view inFunctionBody
        if inBody
            -- Funcions can be recursive (also mutually recursive), so we
            -- cannot add a dependency for f (since this could introduce a
            -- cycle in the dependency graph). However, we add a dependency
            -- to the signature of f, because we will need to know the type
            -- of f for type checking.
            then return [DepFunctionSignature f]

            -- Within all other definitions, we depend on the definition of
            -- function f. This is necessary, because for evaluation of the
            -- expression containing f, the function body must be known.
            -- Also, the functions called by f must be known as well.
            else do
                funcs <- view functions
                return (fmap DepFunction (referencedFunctions funcs f))
    DefComponent name tyName -> return [DepComponent name tyName]


-- | @referencedFunctions f@ returns a list of all functions called by @f@.
-- The result will also contain @f@ itself.
referencedFunctions :: Map Name UFunction -> UFunction -> [UFunction]
referencedFunctions funcs f = Set.toList (execState (go f) Set.empty)
  where
    go f' = do
        visited <- get
        unless (f' `Set.member` visited) $ do
            modify (Set.insert f')
            for_ (functionVars (functionBody f')) go

    functionVars e = catMaybes
        (fmap ((`Map.lookup` funcs) . unLoc) (toList (identsInExpr e)))


identsInSignature :: UFunction -> Set (Loc Name)
identsInSignature (Function _ args ty _) =
    Set.unions (identsInType ty : fmap identsInParam (toList args))
  where
    identsInParam (Parameter _ paramTy) = identsInType paramTy


identsInType :: UType -> Set (Loc Name)
identsInType = Set.unions . fmap identsInExpr . toList


-- | @identsInExpr e@ returns a set of all unbound identifiers in @e@.
identsInExpr :: LExpr -> Set (Loc Name)
identsInExpr = go Set.empty
  where
    go bound e = case unLoc e of
        Identifier name
            | name `Set.member` bound -> Set.empty
            | otherwise -> Set.singleton (name `withLocOf` e)
        Quantified _ name _ e' -> go (Set.insert name bound) e'
        _ -> Set.unions (fmap (go bound) (children e))


parameterSet :: Function expr -> Set Name
parameterSet = Set.fromList . fmap (unLoc . paramName) . toList . functionParams


type Analyzer a
     = ReaderT AnalyzerInfo (StateT DependencyGraph (Either Syntax.Error)) a


runAnalyzer :: Identifiers -> Analyzer () -> Either Syntax.Error DependencyGraph
runAnalyzer idents m =
    execStateT (runReaderT m info) Map.empty
  where
    info  = AnalyzerInfo idents Set.empty funcs Nothing False
    funcs = Map.fromAscList (catMaybes (fmap getFunc (Map.toAscList idents)))
    getFunc (name, def) = case def of
        Loc (DefFunction f) _ -> Just (name, f)
        _                     -> Nothing


-- | @newDependency dep rgn m@ introduces a new dependency @dep@ defined in
-- the location @rgn@ and adds it to the dependency graph. Within the
-- analyzer @m@, the dependencies of @m@ should be defined.
newDependency :: Dependency -> Region -> Analyzer a -> Analyzer a
newDependency dep rgn m = do
    let lDep = Loc dep rgn
    modify (Map.insertWith Set.union lDep Set.empty)
    local (currentDependency .~ Just lDep) m


-- | @dependOn dep rgn@ states that the current dependency depends on the
-- given dependency @dep@.
dependOn :: Dependency -> Region -> Analyzer ()
dependOn dep rgn = view currentDependency >>= \case
    Just this ->
        modify (Map.insertWith Set.union this (Set.singleton (Loc dep rgn)))
    Nothing   -> error $
        "dependOn: not inside a dependency\narguments: " ++ show dep ++ ", " ++
        show rgn
