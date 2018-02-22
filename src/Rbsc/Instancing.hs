{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}


module Rbsc.Instancing where


-- 1. build system instance from system blocks
-- 2. run completer over incomplete system
-- 3. generate constants from system instances
-- 4. evaluate constraints for each instance


import Control.Lens
import Control.Monad.State.Strict

import qualified Data.Map.Strict as Map

import Data.Foldable


import Rbsc.Data.Component
import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.System
import Rbsc.Data.Type

import Rbsc.Report.Error
import Rbsc.Report.Region

import Rbsc.Syntax.Typed hiding (Type(..))



generateInstances :: TModel -> ModelInfo -> Either Error [(System, ModelInfo)]
generateInstances = undefined


pattern TyComponent' :: TypeName -> Type Component
pattern TyComponent' tyName <- TyComponent (toList -> [tyName])


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
