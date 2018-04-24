{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}


-- | Internal functions for the @Instantiation@ module.
module Rbsc.Instantiation.Internal
    ( ArrayInfo(..)
    , Result(..)

    , updateModelInfo
    , buildSystem
    , checkConstraints
    , checkCompartmentUpperBounds
    ) where


import Control.Lens
import Control.Monad.State.Strict

import           Data.List       (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set

import Data.Foldable
import Data.Monoid
import Data.Ord
import Data.Text        (pack)
import Data.Traversable


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


-- | Information about an array of components.
data ArrayInfo = ArrayInfo
    { arrName       :: !Name
    , _arrTyName    :: !TypeName
    , _arrItemNames :: Array Name
    }


-- | A result produced by 'buildSystem'.
data Result = Result
    { _system          :: System
    , _constraints     :: [LSomeExpr]
    , _componentArrays :: [ArrayInfo]
    }

makeLenses ''Result


pattern TyComponent' :: TypeName -> Type Component
pattern TyComponent' tyName <- TyComponent (toList -> [tyName])


pattern LitComponent :: Name -> TypeName -> Expr Component
pattern LitComponent name tyName <- (componentLiteral -> Just (name, tyName))


-- | Extract the 'System' definition and the list of other constraints from
-- the system block.
--
-- Expressions of the form @x : t@, @x[n] : t@, @x boundto y@ and @x in y@ are
-- transformed into the 'System' instance. All other expressions are added
-- to the constraints list.
buildSystem ::
       RecursionDepth -> TModel -> ModelInfo -> Either Error Result
buildSystem depth model info = do
    -- Partition expressions into instantiations of components and other
    -- constraints.
    Result sys constrs arrayInfos <-
        execStateT (traverse insertDefinition (modelSystem model)) initState

    -- Partially evaluate constraints to remove quantifiers. This is done
    -- in order to provide as much information to the 'completeSystem'
    -- function as possible. Partial evaluation will reduce quantification
    -- over a set of components to either true or false (since our
    -- constants table does not contain any components yet). However, this
    -- is fine, since the partially evaluated constraints are only used to
    -- extract the 'boundto' and 'in' relations, but not for evaluating the
    -- constraints.
    constrs' <- traverse (reduceSomeExpr (view constants info) depth) constrs

    -- Split conjunctions into individual constraints.
    let constrs'' = concatMap clauses constrs'

    -- Extract relations 'boundto' and 'in' from the partially evaluated
    -- constraints.
    let compTys = view componentTypes info
    sys' <- execStateT (traverse (insertRelation compTys) constrs'') sys

    return (Result sys' constrs arrayInfos)
  where
    insertDefinition :: LSomeExpr -> StateT Result (Either Error) ()
    insertDefinition e@(Loc (SomeExpr e' _) _) = case e' of
        HasType (Identifier name _) tyName ->
            system.instances.at name .= Just tyName

        HasType (Index (Identifier name _) idx) tyName -> do
            -- upper bound is already checked in TypeChecker.ModelInfo
            idx' <- lift (eval (view constants info) depth idx)
            arr <- for [0 .. idx' - 1] $ \i -> do
                let name' = indexedName name i
                system.instances.at name' .= Just tyName
                return name'
            modifying componentArrays (ArrayInfo name tyName (fromList arr) :)

        _ -> modifying constraints (e :)

    insertRelation ::
        ComponentTypes -> LSomeExpr -> StateT System (Either Error) ()
    insertRelation compTys (Loc (SomeExpr e _) _) = case e of
        BoundTo
            (Loc (LitComponent roleName roleTyName) rgnRole)
            (Loc (LitComponent playerName playerTyName) rgnPlayer)
                | not (isRoleType roleTyName) ->
                    throw rgnRole NotARole
                | not (canPlayRole compTys playerTyName roleTyName) ->
                    throw rgnPlayer (InvalidBinding roleTyName playerTyName)
                | otherwise ->
                    boundTo.at roleName .= Just playerName

        Element
            (Loc (LitComponent roleName roleTyName) rgnRole)
            (Loc (LitComponent compartmentName compTyName) rgnComp)
                | not (isRoleType roleTyName) ->
                    throw rgnRole NotARole
                | not (isCompartmentType compTyName) ->
                    throw rgnComp NotACompartment
                | otherwise ->
                    containedIn.at roleName .= Just compartmentName

        _ -> return ()

    initState = Result emptySystem [] []

    emptySystem = System Map.empty Map.empty Map.empty

    isRoleType tyName = has (componentTypes.at tyName._Just._RoleType) info
    isCompartmentType tyName =
        has (componentTypes.at tyName._Just._CompartmentType) info

    canPlayRole compTys playerTyName roleTyName =
        case Map.lookup roleTyName compTys of
            Just (RoleType roleTyNames) -> playerTyName `Set.member` roleTyNames
            _ -> False


-- | If an expression is an identifier or an indexed indentifier with type
-- 'TyComponent', return the component 'Name' and its 'TypeName'. In case
-- of an indexed identifier, the index is added to the component's name.
componentLiteral :: Expr Component -> Maybe (Name, TypeName)
componentLiteral = \case
    Identifier name (TyComponent' tyName) -> Just (name, tyName)
    Index
        (Identifier name (TyArray _ (TyComponent' tyName)))
        (Loc (Literal idx) _) ->
            Just (indexedName name idx, tyName)
    _ -> Nothing


-- indexedName is guaranteed to be unused, because brackets
-- are not allowed in identifier names
indexedName :: Name -> Integer -> Name
indexedName name i = name <> "[" <> pack (show i) <> "]"


-- | Updates a 'ModelInfo' by adding constants for all the 'Component's
-- contained in the given 'System'.
updateModelInfo :: ModelInfo -> [ArrayInfo] -> System -> (System, ModelInfo)
updateModelInfo info arrayInfos sys =
    (sys, over constants (Map.union consts) info)
  where
    consts = Map.union arrays componentConsts
    componentConsts = generateConstants sys instances'
    (arrays, instances') = generateArrays sys arrayInfos


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


-- | Checks whether the given list of constraints is satisfied by the model
-- with the given 'ModelInfo'.
checkConstraints ::
       RecursionDepth -> [LSomeExpr] -> ModelInfo -> Either Error Bool
checkConstraints depth cs info =
    evalConstraints depth (view constants info) cs


evalConstraints ::
       RecursionDepth -> Constants -> [LSomeExpr] -> Either Error Bool
evalConstraints depth consts cs = and <$> traverse evalConstraint cs
  where
    evalConstraint = \case
        Loc (SomeExpr e TyBool) rgn -> eval consts depth (Loc e rgn)
        _ -> error "evalConstraint: type error"


reduceSomeExpr ::
       Constants -> RecursionDepth -> LSomeExpr -> Either Error LSomeExpr
reduceSomeExpr consts depth (Loc (SomeExpr e ty) rgn) = do
    e' <- reduce consts depth (Loc e rgn)
    return (Loc (SomeExpr e' ty) rgn)


clauses :: LSomeExpr -> [LSomeExpr]
clauses = \case
    e@(Loc (SomeExpr e' TyBool) rgn) -> case e' of
        LogicOp And l r ->
            clauses (Loc (SomeExpr l TyBool) rgn) ++
            clauses (Loc (SomeExpr r TyBool) rgn)
        Literal True -> []
        _ -> [e]
    e -> [e]


-- | Check if the given 'System' violates the role cardinalities given by
-- the compartment type definitions. If so, an error is thrown.
checkCompartmentUpperBounds :: ComponentTypes -> System -> Either Error ()
checkCompartmentUpperBounds compTys sys =
    for_ (view (instances.to Map.assocs) sys) $ \(name, tyName) ->
        case Map.lookup tyName compTys of
            Just (CompartmentType roleRefLists) ->
                checkCompartment sys name roleRefLists
            _ -> return ()


checkCompartment :: System -> Name -> [[RoleRef]] -> Either Error ()
checkCompartment sys name roleRefLists
    | any Map.null overfulls = return ()
    | otherwise = case sortedOverfulls of
        (os:_) -> throw dummyRegion (TooManyRoles name (Map.assocs os))
        _      -> return ()
  where
    sortedOverfulls = sortBy (comparing numAdditional) overfulls
    overfulls = fmap (getOverfullTypes sys name) roleRefLists

    numAdditional = Map.foldr (+) 0

    -- A 'TooManyRoles' error cannot be given a precise code region, since
    -- it is potentially a result of multiple constraints in the system
    -- block.
    dummyRegion = Region "" "" (Position 1 1) (Position 1 2)


getOverfullTypes :: System -> Name -> [RoleRef] -> Map TypeName Int
getOverfullTypes sys name roleRefs =
    Map.filter (> 0) (Map.unionWith (-) numContained numMaxRequired')
  where
    numMaxRequired' = Map.intersection numMaxRequired numContained
    numMaxRequired  = Map.unionsWith (+) (fmap fromRoleRef roleRefs)

    fromRoleRef (RoleRef roleTyName (_, upper)) = Map.singleton roleTyName upper

    numContained =
        Map.unionsWith (+) (fmap (`Map.singleton` (1 :: Int)) containedTypes)

    containedTypes =
        mapMaybe (\n -> view (instances.at n) sys) (containedRoles name sys)
