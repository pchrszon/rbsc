{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}


-- | Instantiation of system instances.
module Rbsc.Instancing
    ( generateInstances
    ) where


import Control.Lens
import Control.Monad.State.Strict

import           Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Data.Foldable
import Data.Monoid
import Data.Text        (pack)
import Data.Traversable


import Rbsc.Completer

import Rbsc.Data.Array
import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))


data ArrayInfo = ArrayInfo
    { arrName       :: !Name
    , _arrTyName    :: !TypeName
    , _arrItemNames :: Array Name
    }

data BuilderState = BuilderState
    { _system          :: System
    , _constraints     :: [LSomeExpr]
    , _componentArrays :: [ArrayInfo]
    }

makeLenses ''BuilderState


-- | Generate all system instances that fulfill the constraints defined in
-- the system block of the model. For each system, an extended 'ModelInfo'
-- is returned that contains constants for each component instance.
generateInstances ::
       RecursionDepth
    -> TModel
    -> ModelInfo
    -> Either Error [(System, ModelInfo)]
generateInstances depth model info = do
    BuilderState sys cs arrayInfos <- buildSystem depth model info
    let syss = rights (completeSystem (view componentTypes info) sys) -- TODO: cycle warnings
        sysInfos = fmap (updateModelInfo info arrayInfos) syss
    filterM (checkConstraints depth cs . snd) sysInfos


updateModelInfo :: ModelInfo -> [ArrayInfo] -> System -> (System, ModelInfo)
updateModelInfo info arrayInfos sys =
    (sys, over constants (Map.union consts) info)
  where
    consts = Map.union arrays componentConsts
    componentConsts = generateConstants sys instances'
    (arrays, instances') = generateArrays sys arrayInfos


checkConstraints ::
       RecursionDepth -> [LSomeExpr] -> ModelInfo -> Either Error Bool
checkConstraints depth cs info =
    evalConstraints depth (view constants info) cs


pattern TyComponent' :: TypeName -> Type Component
pattern TyComponent' tyName <- TyComponent (toList -> [tyName])


-- | Extract the 'System' definition and the list of other constraints from
-- the system block.
--
-- Expressions of the form @x : t@, @x[n] : t@, @x boundto y@ and @x in y@ are
-- transformed into the 'System' instance. All other expressions are added
-- to the constraints list.
buildSystem ::
       RecursionDepth -> TModel -> ModelInfo -> Either Error BuilderState
buildSystem depth model info =
    execStateT (traverse insert (modelSystem model)) initState
  where
    insert :: LSomeExpr -> StateT BuilderState (Either Error) ()
    insert e@(Loc (SomeExpr e' _) _) = case e' of
        HasType (Identifier name _) tyName ->
            system.instances.at name .= Just tyName

        HasType (Index (Identifier name _) idx) tyName -> do
            -- upper bound is already checked in TypeChecker.ModelInfo
            idx' <- lift (eval (view constants info) depth idx)
            arr <- for [0 .. idx' - 1] $ \i -> do
                -- indexedName is guaranteed to be unused, because brackets
                -- are not allowed in identifier names
                let indexedName = name <> "[" <> pack (show i) <> "]"
                system.instances.at indexedName .= Just tyName
                return indexedName
            modifying componentArrays ((ArrayInfo name tyName (fromList arr)) :)

        BoundTo
            (Loc (Identifier roleName (TyComponent' roleTyName)) rgn)
            (Identifier playerName _)
                | isRoleType roleTyName ->
                    system.boundTo.at roleName .= Just playerName
                | otherwise -> throw rgn NotARole

        Element
            (Loc (Identifier roleName (TyComponent' roleTyName)) rgnRole)
            (Loc (Identifier compartmentName (TyComponent' compTyName)) rgnComp)
                | not (isRoleType roleTyName) ->
                    throw rgnRole NotARole
                | not (isCompartmentType compTyName) ->
                    throw rgnComp NotACompartment
                | otherwise ->
                    system.containedIn.at roleName .= Just compartmentName

        _ -> modifying constraints (e :)

    initState = BuilderState emptySystem [] []

    emptySystem = System Map.empty Map.empty Map.empty

    isRoleType tyName = has (componentTypes.at tyName._Just._RoleType) info
    isCompartmentType tyName =
        has (componentTypes.at tyName._Just._CompartmentType) info


generateConstants :: System -> Map Name TypeName -> Constants
generateConstants sys = Map.mapWithKey generateConstant
  where
    generateConstant name tyName =
        let comp = componentForName sys tyName name
        in SomeExpr (Literal comp) (TyComponent (Set.singleton tyName))


generateArrays :: System -> [ArrayInfo] -> (Constants, Map Name TypeName)
generateArrays sys arrayInfos =
    (Map.fromList (fmap generateArray arrayInfos), instances')
  where
    generateArray (ArrayInfo name tyName componentNames) =
        let arr = fmap (componentForName sys tyName) componentNames
            ty  = TyArray (bounds arr) (TyComponent (Set.singleton tyName))
            e   = SomeExpr (Literal arr) ty
        in (name, e)

    instances' = foldr Map.delete (view instances sys) (fmap arrName arrayInfos)


componentForName :: System -> TypeName -> Name -> Component
componentForName sys tyName name =
    let mBoundTo = view (boundTo.at name) sys
        mContainedIn = view (containedIn.at name) sys
    in Component name tyName mBoundTo mContainedIn


evalConstraints ::
       RecursionDepth -> Constants -> [LSomeExpr] -> Either Error Bool
evalConstraints depth consts cs = and <$> traverse evalConstraint cs
  where
    evalConstraint = \case
        Loc (SomeExpr e TyBool) rgn -> eval consts depth (Loc e rgn)
        _ -> error "evalConstraint: type error"
