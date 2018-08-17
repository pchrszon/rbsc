{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}


-- | This module provides functions to extract all identifiers and their
-- definitions from the model.
module Rbsc.TypeChecker.Identifiers
    ( Identifiers

    , IdentifierDef(..)
    , _DefConstant
    , _DefFunction
    , _DefComponentType
    , _DefComponent

    , ComponentTypeDef(..)
    , _TypeDefNatural
    , _TypeDefRole
    , _TypeDefCompartment

    , ComponentDef(..)

    , identifierDefs
    ) where


import Control.Lens
import Control.Monad.State.Strict

import Data.Function
import Data.Ord
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


import Rbsc.Data.Scope

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Untyped


-- | A Map of all definitions of identifiers within the model.
type Identifiers = Map ScopedName (Loc IdentifierDef)

-- | The definition of an identifier.
data IdentifierDef
    = DefConstant UConstant -- ^ the identifier represents a constant
    | DefFunction UFunction -- ^ the identifier represents a function
    | DefGlobal UVarDecl -- ^ the identifier represents a global variable
    | DefLocal !TypeName UVarDecl -- ^ the identifier represents a local variable
    | DefComponentType ComponentTypeDef -- ^ the identifier represents a component type
    | DefComponent ComponentDef -- ^ the identifier represents a component or a component array
    deriving (Eq, Ord, Show)


-- | The definition of a component type.
data ComponentTypeDef
    = TypeDefNatural NaturalTypeDef
    | TypeDefRole RoleTypeDef
    | TypeDefCompartment UCompartmentTypeDef
    deriving (Eq, Ord, Show)


-- | The definition of a component instance or a component array.
data ComponentDef = ComponentDef
    { compDefName     :: !(Loc Name)
    , compDefTypeName :: !(Loc TypeName)
    , compDefLength   :: Maybe LExpr
    } deriving (Show)

instance Eq ComponentDef where
    (==) = (==) `on` compDefName

instance Ord ComponentDef where
    compare = comparing compDefName


makePrisms ''IdentifierDef
makePrisms ''ComponentTypeDef


data BuilderState = BuilderState
    { _identifiers :: Identifiers
    , _errors      :: [Error]
    }

makeLenses ''BuilderState


-- | Extract all identifiers together with their definition from the
-- 'Model'. In case one or more identifiers are defined multiple times,
-- a list of 'DuplicateIdentifier' errors is returned.
identifierDefs :: Model -> Either [Error] Identifiers
identifierDefs Model{..} = runBuilder $ do
    insertConstants modelConstants
    insertFunctions modelFunctions
    insertGlobals modelGlobals
    insertComponentTypes ntdName TypeDefNatural modelNaturalTypes
    insertComponentTypes rtdName TypeDefRole modelRoleTypes
    insertComponentTypes ctdName TypeDefCompartment modelCompartmentTypes
    insertComponents modelSystem
    insertLocalVars modelImpls
    insertCoordinatorVars modelCoordinators


insertConstants :: [UConstant] -> Builder ()
insertConstants = traverse_ (insert Global <$> constName <*> DefConstant)


insertFunctions :: [UFunction] -> Builder ()
insertFunctions =
    traverse_ (insert Global <$> functionName <*> DefFunction)


insertGlobals :: [UVarDecl] -> Builder ()
insertGlobals = traverse_ (insertVarDecl Global DefGlobal)


insertComponentTypes ::
       (a -> Loc TypeName) -> (a -> ComponentTypeDef) -> [a] -> Builder ()
insertComponentTypes getName con =
    traverse_
        (insert Global <$> (fmap getTypeName . getName) <*>
         (DefComponentType . con))


insertComponents :: [LExpr] -> Builder ()
insertComponents es = for_ es $ \case
    HasType' (Loc (Identifier name) rgn) tyName ->
        insert Global (Loc name rgn)
            (DefComponent (ComponentDef (Loc name rgn) tyName Nothing))
    HasType' (Index' (Loc (Identifier name) rgn) len) tyName ->
        insert Global (Loc name rgn)
            (DefComponent (ComponentDef (Loc name rgn) tyName (Just len)))
    _ -> return ()


insertLocalVars :: Map TypeName [UNamedModuleBody] -> Builder ()
insertLocalVars = traverse_ insertVarsForType . Map.assocs
  where
    insertVarsForType (tyName, bodies) =
        for_ bodies $ \(NamedModuleBody _ body) ->
            for_ (bodyVars body) $
                insertVarDecl (Local tyName) (DefLocal tyName)


insertVarDecl :: Scope -> (UVarDecl -> IdentifierDef) -> UVarDecl -> Builder ()
insertVarDecl sc c decl = do
    insert sc (declName decl) (c decl)
    insertVarType (declType decl)


insertVarType :: UVarType -> Builder ()
insertVarType = \case
    VarTyEnum names ->
        insertConstants (fmap mkConstant (zip names [0 ..]))
    _ -> return ()
  where
    mkConstant (name, i) =
        Constant name (Just TyInt) (LitInt i `withLocOf` name)


insertCoordinatorVars :: [UCoordinator] -> Builder ()
insertCoordinatorVars = traverse_ insertVars
  where
    insertVars coord = insertGlobals (coordVars coord)


type Builder a = State BuilderState a


runBuilder :: Builder a -> Either [Error] Identifiers
runBuilder m
    | null errs = Right idents
    | otherwise = Left errs
  where
    BuilderState idents errs = execState m (BuilderState Map.empty [])


insert :: Scope -> Loc Name -> IdentifierDef -> Builder ()
insert sc (Loc name rgn) def =
    use (identifiers.at (ScopedName sc name)) >>= \case
        Just def' -> throw' (locError rgn (DuplicateIdentifier (getLoc def')))
        Nothing -> identifiers.at (ScopedName sc name) .= Just (Loc def rgn)


throw' :: Error -> Builder ()
throw' e = modifying errors (++ [e])
