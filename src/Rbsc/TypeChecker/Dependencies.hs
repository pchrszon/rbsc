{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
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


import Rbsc.Data.ComponentType
import Rbsc.Data.Scope

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Untyped

import Rbsc.TypeChecker.Identifiers

import Rbsc.Util


-- | A @Dependency@ corresponds to an 'IdentifierDef'. Each @Dependency@
-- may itself be dependend on other dependencies.
data Dependency
    = DepDefinition IdentifierDef
    | DepFunctionSignature UFunction
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
sortDefinitions :: Identifiers -> Either Error [Dependency]
sortDefinitions idents = do
    depGraph <- runAnalyzer idents
                            (traverse_ (insert . unLoc) (Map.elems idents))
    case topoSort (Map.keys depGraph) (lookupEdges depGraph) of
        Right deps     -> return (fmap unLoc deps)
        Left  depCycle -> throwError (buildError depCycle)
  where
    lookupEdges depGraph dep =
        toList (Map.findWithDefault Set.empty dep depGraph)

    buildError (Loc dep rgn :| deps) = locError rgn
        (CyclicDefinition (getConstructName dep) (fmap getLoc deps))

    getConstructName = \case
        DepDefinition def -> case def of
            DefConstant _                            -> "constant"
            DefFunction _                            -> "function"
            DefGlobal _                              -> "global variable"
            DefLocal _ _                             -> "local variable"
            DefComponentType _                       -> "type"
            DefComponent (ComponentDef _ _ Nothing)  -> "component"
            DefComponent (ComponentDef _ _ (Just _)) -> "component array"
        DepFunctionSignature _ -> "function"


-- | A dependency graph.
--
-- Each key maps to a set of other definitions it depends on.
type DependencyGraph = Map LDependency (Set LDependency)


type LDependency = Loc Dependency


-- | Insert an 'IdentifierDef' into the dependency graph.
insert :: IdentifierDef -> Analyzer ()
insert def = case def of
    DefConstant c        -> insertConstant c
    DefFunction f        -> insertFunction f
    DefGlobal g          -> insertGlobal g
    DefLocal tyName decl -> insertLocal tyName decl
    DefComponentType t   -> insertComponentType t
    DefComponent c       -> insertComponents c


insertConstant :: UConstant -> Analyzer ()
insertConstant c@(Constant (Loc _ rgn) sTy e) =
    newDependency (DepDefinition (DefConstant c)) rgn $ do
        mIdentsTy <- _Just identsInType sTy
        idents <- identsInExpr e
        dependOnIdentifiers (idents `Set.union` fromMaybe Set.empty mIdentsTy)


insertFunction :: UFunction -> Analyzer ()
insertFunction f@(Function (Loc _ rgn) _ _ e) = do
    newDependency (DepFunctionSignature f) rgn $ do
        identsSig <- identsInSignature f
        dependOnIdentifiers identsSig
    newDependency (DepDefinition (DefFunction f)) rgn $ do
        dependOn (DepFunctionSignature f) rgn
        local ((inFunctionBody .~ True) . (parameters .~ parameterSet f)) $ do
            idents <- identsInExpr e
            dependOnIdentifiers idents


insertGlobal :: UVarDecl -> Analyzer ()
insertGlobal decl@(VarDecl (Loc _ rgn) vTy _) =
    newDependency (DepDefinition (DefGlobal decl)) rgn $ do
        identsTy <- identsInVarType vTy
        dependOnIdentifiers identsTy


insertLocal :: TypeName -> UVarDecl -> Analyzer ()
insertLocal tyName decl@(VarDecl (Loc _ rgn) vTy _) =
    newDependency (DepDefinition (DefLocal tyName decl)) rgn $ do
        identsTy <- identsInVarType vTy
        dependOnIdentifiers identsTy


insertComponentType :: ComponentTypeDef -> Analyzer ()
insertComponentType t =
    newDependency (DepDefinition (DefComponentType t)) rgn $ do
        locals <- getLocalVars
        for_ locals $ \decl ->
            dependOn (DepDefinition (DefLocal tyName decl)) rgn

        case t of
            TypeDefCompartment (CompartmentTypeDef _ multiRoleLists) ->
                for_ multiRoleLists $ \multiRoles ->
                    for_ multiRoles $ \(MultiRole _ mBounds) -> case mBounds of
                        Just (lower, upper) -> do
                            identsLower <- identsInExpr lower
                            identsUpper <- identsInExpr upper
                            dependOnIdentifiers identsLower
                            dependOnIdentifiers identsUpper
                        Nothing -> return ()
            _ -> return ()
  where
    Loc tyName rgn = case t of
        TypeDefNatural nt     -> ntdName nt
        TypeDefRole rt        -> rtdName rt
        TypeDefCompartment ct -> ctdName ct

    getLocalVars :: Analyzer [UVarDecl]
    getLocalVars =
        mapMaybe (getLocalVar . unLoc) . Map.elems <$> view identifiers

    getLocalVar :: IdentifierDef -> Maybe UVarDecl
    getLocalVar = \case
        DefLocal tyName' decl | tyName' == tyName -> Just decl
        _ -> Nothing

insertComponents :: ComponentDef -> Analyzer ()
insertComponents c@(ComponentDef (Loc _ rgn) tyName mLen) =
    newDependency (DepDefinition (DefComponent c)) rgn $ do
        dependOnIdentifier (fmap getTypeName tyName)
        case mLen of
            Just len -> do
                idents <- identsInExpr len
                dependOnIdentifiers idents
            Nothing -> return ()


-- | @dependOnIdentifiers idents@ states that the current definition (see
-- 'newDependency') depends on all the identifiers contained in @idents@.
dependOnIdentifiers :: Foldable t => t (Loc Name) -> Analyzer ()
dependOnIdentifiers = traverse_ dependOnIdentifier . toList


dependOnIdentifier :: Loc Name -> Analyzer ()
dependOnIdentifier (Loc name rgn) = do
    params <- view parameters

    -- If the identifier is bound by a parameter, we skip it, since it does
    -- not depend on any identifier definition.
    unless (name `Set.member` params) $
        -- We only check global identifiers, because no definition can
        -- directly depend on a local variable without a qualifier.
        view (identifiers.at (ScopedName Global name)) >>= \case
            Just (Loc def _) -> do
                deps <- depsFromIdentifierDef def
                for_ deps $ \dep ->
                    dependOn dep rgn
            Nothing -> throw rgn UndefinedIdentifier


depsFromIdentifierDef :: IdentifierDef -> Analyzer [Dependency]
depsFromIdentifierDef = \case
    DefFunction f -> do
        inBody <- view inFunctionBody
        if inBody
            -- Functions can be recursive (also mutually recursive), so we
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
                ref <- referencedFunctions f
                return (fmap (DepDefinition . DefFunction) ref)
    def -> return [DepDefinition def]


-- | @referencedFunctions f@ returns a list of all functions called by @f@.
-- The result will also contain @f@ itself.
referencedFunctions :: UFunction -> Analyzer [UFunction]
referencedFunctions f = Set.toList <$> execStateT (go f) Set.empty
  where
    go f' = do
        visited <- get
        unless (f' `Set.member` visited) $ do
            modify (Set.insert f')
            fvars <- functionVars (functionBody f')
            for_ fvars go

    functionVars e = do
        idents <- lift (identsInExpr e)
        funcs  <- lift (view functions)
        return (mapMaybe ((`Map.lookup` funcs) . unLoc) (toList idents))


identsInSignature :: UFunction -> Analyzer (Set (Loc Name))
identsInSignature (Function _ args ty _) = do
    identsTy <- identsInType ty
    identsParams <- traverse identsInParam (toList args)
    return (Set.unions (identsTy : identsParams))
  where
    identsInParam (Parameter _ paramTy) = identsInType paramTy


identsInType :: UType -> Analyzer (Set (Loc Name))
identsInType ty = case ty of
    TyComponent tySet -> identsInComponentTypeSet tySet
    TyArray r ty'     -> Set.union <$> identsInRange r <*> identsInType ty'
    TyFunc tyL tyR    -> Set.union <$> identsInType tyL <*> identsInType tyR
    _                 -> return Set.empty


identsInComponentTypeSet :: ComponentTypeSet -> Analyzer (Set (Loc Name))
identsInComponentTypeSet cts = do
    idents <- view (identifiers.to Map.assocs)
    return $ case cts of
        AllComponents -> filterIdents _DefComponentType idents
        AllNaturals   -> filterIdents (_DefComponentType._TypeDefNatural) idents
        AllRoles      -> filterIdents (_DefComponentType._TypeDefRole) idents
        AllCompartments ->
            filterIdents (_DefComponentType._TypeDefCompartment) idents
        ComponentTypeSet tySet -> Set.map (fmap getTypeName) tySet
  where
    filterIdents p = Set.fromList . fmap toIdent . filter (has (_2.to unLoc.p))
    toIdent (ScopedName _ name, Loc _ rgn) = Loc name rgn


identsInVarType :: UVarType -> Analyzer (Set (Loc Name))
identsInVarType = \case
    VarTyInt (lower, upper) ->
        Set.union <$> identsInExpr lower <*> identsInExpr upper
    VarTyArray r ty' -> Set.union <$> identsInRange r <*> identsInVarType ty'
    _ -> return Set.empty


identsInRange :: (LExpr, LExpr) -> Analyzer (Set (Loc Name))
identsInRange (lower, upper) =
    Set.union <$> identsInExpr lower <*> identsInExpr upper


-- | @identsInExpr e@ returns a set of all unbound identifiers in @e@.
identsInExpr :: LExpr -> Analyzer (Set (Loc Name))
identsInExpr = go False Set.empty
  where
    go inAction bound e = case unLoc e of
        LitAction e' -> go True bound e'
        GenArray e' name lower upper -> do
            idents <- go inAction (Set.insert name bound) e'
            identsLower <- go inAction bound lower
            identsUpper <- go inAction bound upper
            return (Set.unions [idents, identsLower, identsUpper])
        Identifier name -> do
            idents <- view identifiers
            if  | name `Set.member` bound ->
                    return Set.empty
                | inAction && Map.notMember (ScopedName Global name) idents ->
                    return Set.empty
                | otherwise ->
                    return (Set.singleton (name `withLocOf` e))
        HasType e' tyName ->
            Set.insert (fmap getTypeName tyName) <$> go inAction bound e'
        Quantified _ name qdTy e' -> do
            idents <- case qdTy of
                QdTypeComponent tySet ->
                    identsInComponentTypeSet tySet
                QdTypeInt (lower, upper) ->
                    Set.union
                        <$> go inAction bound lower
                        <*> go inAction bound upper
            Set.union idents <$> go inAction (Set.insert name bound) e'
        _ -> Set.unions <$> traverse (go inAction bound) (children e)


parameterSet :: Function expr -> Set Name
parameterSet = Set.fromList . fmap (unLoc . paramName) . toList . functionParams


type Analyzer a
     = ReaderT AnalyzerInfo (StateT DependencyGraph (Either Error)) a


runAnalyzer :: Identifiers -> Analyzer () -> Either Error DependencyGraph
runAnalyzer idents m =
    execStateT (runReaderT m info) Map.empty
  where
    info  = AnalyzerInfo idents Set.empty funcs Nothing False
    funcs = Map.fromAscList (mapMaybe getFunc (Map.toAscList idents))
    getFunc (ScopedName _ name, def) = case def of
        Loc (DefFunction f) _ -> Just (name, f)
        _                     -> Nothing


-- | @newDependency dep rgn m@ introduces a new dependency @dep@ defined in
-- the location @rgn@ and adds it to the dependency graph. Within the
-- analyzer @m@, the dependencies of @m@ should be defined.
newDependency :: Dependency -> Region -> Analyzer a -> Analyzer a
newDependency dep rgn m = do
    let lDep = Loc dep rgn
    modify (Map.insertWith Set.union lDep Set.empty)
    local (currentDependency ?~ lDep) m


-- | @dependOn dep rgn@ states that the current dependency depends on the
-- given dependency @dep@.
dependOn :: Dependency -> Region -> Analyzer ()
dependOn dep rgn = view currentDependency >>= \case
    Just this ->
        modify (Map.insertWith Set.union this (Set.singleton (Loc dep rgn)))
    Nothing   -> error $
        "dependOn: not inside a dependency\narguments: " ++ show dep ++ ", " ++
        show rgn
