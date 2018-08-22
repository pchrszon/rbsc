{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Rbsc.CompleterSpec (spec) where


import Control.Lens hiding (elements)

import           Data.List (sort)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Data.Text (Text, toLower)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck


import Rbsc.Completer

import Rbsc.Data.ComponentType
import Rbsc.Data.ModelInfo
import Rbsc.Data.Name
import Rbsc.Data.System

import Rbsc.Util


spec :: Spec
spec = describe "completeSystem" $ parallel $ do
    prop "binds all roles" $ \(Model info sys) ->
        allOf
            (traverse._Right)
            (rolesAreBound (view componentTypes info))
            (completeSystem sys info)

    prop "fills all compartments" $ \(Model info sys) ->
        allOf (traverse._Right)
              (compartmentsAreFilled (view componentTypes info))
              (completeSystem sys info)


rolesAreBound :: ComponentTypes -> System -> Bool
rolesAreBound types sys =
    Set.filter (isRole types sys) (Map.keysSet (view instances sys)) ==
    Map.keysSet (view boundTo sys)


isRole :: ComponentTypes -> System -> ComponentName -> Bool
isRole types sys name = case view (instances.at name) sys of
    Just tyName -> case Map.lookup tyName types of
        Just (RoleType _) -> True
        _                 -> False
    _ -> False


compartmentsAreFilled :: ComponentTypes -> System -> Bool
compartmentsAreFilled types sys =
    all isFilled (view (instances.to Map.assocs) sys)
  where
    isFilled (name, tyName) =
        case Map.lookup tyName types of
            Just (CompartmentType roleRefLists) ->
                let cs = containedRoles name sys
                    cTys = fmap (`Map.lookup` (sys^.instances)) cs
                    roleTypeLists = fmap (concatMap fromRoleRef) roleRefLists
                in any (\rTys -> sort cTys == sort (fmap Just rTys)) roleTypeLists
            _ -> True

    fromRoleRef (RoleRef tyName (lower, _)) = replicate lower tyName


data Model = Model ModelInfo System deriving (Show)

instance Arbitrary Model where
    arbitrary = do
        types <- genComponentTypes
        is <- genInstances types
        bt <- genBindings types is

        let sys = System
                    { _instances = is
                    , _boundTo = bt
                    , _containedIn = Map.empty
                    }

        return (Model (emptyModelInfo & componentTypes .~ types) sys)


genInstances :: ComponentTypes -> Gen (Map ComponentName TypeName)
genInstances =
    fmap (Map.fromList . fmap mkInstance) . nonEmptySublistOf . Map.keys
  where
    mkInstance tyName = (mkInstanceName tyName, tyName)


genBindings :: ComponentTypes
            -> Map ComponentName TypeName
            -> Gen (Map RoleName ComponentName)
genBindings types is = do
    is' <- sublistOf (Map.assocs is)
    Map.unions <$> traverse (uncurry genBinding) is'
  where
    genBinding name tyName =
        case Map.lookup tyName types of
            Just (RoleType playerTyNames) -> do
                let playerNames =
                        Set.map mkInstanceName playerTyNames
                    possiblePlayers =
                        Set.intersection (Map.keysSet is) playerNames
                if null possiblePlayers
                    then return Map.empty
                    else Map.singleton name <$>
                         elements (Set.toList possiblePlayers)
            _ -> return Map.empty


genComponentTypes :: Gen ComponentTypes
genComponentTypes = scale (min 1) $ do
    naturalTyNames <- typeNames "N"
    roleTyNames <- typeNames "R"
    compartmentTyNames <- typeNames "C"

    -- intentionally left out roles as players since instantiation often
    -- leads to exponential blow-up
    let playerTyNames = naturalTyNames ++ compartmentTyNames

    ntys <- naturalTypes naturalTyNames
    rtys <- roleTypes roleTyNames playerTyNames
    ctys <- compartmentTypes compartmentTyNames roleTyNames

    return (Map.unions [ntys, rtys, ctys])


naturalTypes :: [TypeName] -> Gen ComponentTypes
naturalTypes = pure . Map.fromList . fmap (, NaturalType)


roleTypes :: [TypeName] -> [TypeName] -> Gen ComponentTypes
roleTypes roleTyNames playerTyNames =
    Map.fromList <$> traverse roleType roleTyNames
  where
    roleType n =
        (,) n . RoleType . Set.fromList <$> nonEmptySublistOf playerTyNames


compartmentTypes :: [TypeName] -> [TypeName] -> Gen ComponentTypes
compartmentTypes compartmentTyNames roleTyNames =
    Map.fromList <$> traverse compartmentType compartmentTyNames
  where
    compartmentType n = (,) n . CompartmentType . (: []) <$>
        nonEmptySublistOf (fmap mkRoleRef roleTyNames)

    mkRoleRef tyName = RoleRef tyName (1, 1)


typeNames :: Text -> Gen [TypeName]
typeNames name = do
    upper <- getNonNegative <$> arbitrary
    return (fmap (mkTypeName name) [0..upper])


mkTypeName :: Text -> Integer -> TypeName
mkTypeName n i = TypeName (appendIndex n i)


mkInstanceName :: TypeName -> ComponentName
mkInstanceName = flip ComponentName Nothing . toLower . getTypeName


nonEmptySublistOf :: [a] -> Gen [a]
nonEmptySublistOf xs = sublistOf xs `suchThat` (not . null)
