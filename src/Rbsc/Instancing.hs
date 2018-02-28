{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}


-- | Instantiation of system instances.
module Rbsc.Instancing
    ( generateInstances
    ) where


import Control.Lens
import Control.Monad.State.Strict

import Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Data.Foldable

import Rbsc.Completer

import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Eval

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type (..))


-- | Generate all system instances that fulfill the constraints defined in
-- the system block of the model.
generateInstances ::
       RecursionDepth
    -> TModel
    -> ModelInfo
    -> Either Error [(System, ModelInfo)]
generateInstances depth model info = do
    (sys, constraints) <- buildSystem model info
    let syss = rights (completeSystem (view componentTypes info) sys) -- TODO: cycle warnings
        sysInfos = fmap (updateModelInfo info) syss
    filterM (checkConstraints depth constraints . snd) sysInfos


updateModelInfo :: ModelInfo -> System -> (System, ModelInfo)
updateModelInfo info sys = (sys, info')
  where
    info' = over constants (Map.union (generateConstants sys)) info


checkConstraints ::
       RecursionDepth -> [LSomeExpr] -> ModelInfo -> Either Error Bool
checkConstraints depth cs info =
    evalConstraints depth (view constants info) cs


pattern TyComponent' :: TypeName -> Type Component
pattern TyComponent' tyName <- TyComponent (toList -> [tyName])


-- | Extract the 'System' definition and the list of other constraints from
-- the system block.
--
-- Expressions of the form @x : t@, @x boundto y@ and @x in y@ are
-- transformed into the 'System' instance. All other expressions are added
-- to the constraints list.
buildSystem :: TModel -> ModelInfo -> Either Error (System, [LSomeExpr])
buildSystem model info =
    execStateT (traverse insert (modelSystem model)) (emptySystem, [])
  where
    insert :: LSomeExpr -> StateT (System, [LSomeExpr]) (Either Error) ()
    insert e@(Loc (SomeExpr e' _) _) = case e' of
        HasType (Identifier name _) tyName ->
            _1.instances.at name .= Just tyName
        BoundTo
            (Loc (Identifier roleName (TyComponent' roleTyName)) rgn)
            (Identifier playerName _)
                | isRoleType roleTyName ->
                    _1.boundTo.at roleName .= Just playerName
                | otherwise -> throw rgn NotARole
        Element
            (Loc (Identifier roleName (TyComponent' roleTyName)) rgnRole)
            (Loc (Identifier compartmentName (TyComponent' compTyName)) rgnComp)
                | not (isRoleType roleTyName) ->
                    throw rgnRole NotARole
                | not (isCompartmentType compTyName) ->
                    throw rgnComp NotACompartment
                | otherwise ->
                    _1.containedIn.at roleName .= Just compartmentName
        _ -> modifying _2 (e :)

    emptySystem = System Map.empty Map.empty Map.empty

    isRoleType tyName = has (componentTypes.at tyName._Just._RoleType) info
    isCompartmentType tyName =
        has (componentTypes.at tyName._Just._CompartmentType) info


generateConstants :: System -> Constants
generateConstants sys = Map.mapWithKey generateConstant (view instances sys)
  where
    generateConstant name tyName =
        let mBoundTo = view (boundTo.at name) sys
            mContainedIn = view (containedIn.at name) sys
            comp = Component name tyName mBoundTo mContainedIn
        in SomeExpr (Literal comp) (TyComponent (Set.singleton tyName))


evalConstraints ::
       RecursionDepth -> Constants -> [LSomeExpr] -> Either Error Bool
evalConstraints depth consts cs = and <$> traverse evalConstraint cs
  where
    evalConstraint = \case
        Loc (SomeExpr e TyBool) rgn -> eval consts depth (Loc e rgn)
        _ -> error "evalConstraint: type error"
