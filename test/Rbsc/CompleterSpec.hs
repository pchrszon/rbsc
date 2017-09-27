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
import Rbsc.Util
import Rbsc.Type


spec :: Spec
spec = describe "system instance completion" $ do
    prop "binds all roles" $ \(Model types sys) ->
        allOf (traverse._Right) (rolesAreBound types) (completeSystem types sys)
    prop "fills all compartments" $ \(Model types sys) ->
        allOf (traverse._Right)
              (compartmentsAreFilled types)
              (completeSystem types sys)


rolesAreBound :: ComponentTypes -> System -> Bool
rolesAreBound types sys =
    Set.filter (isRole types sys) (Map.keysSet (view instances sys)) ==
    Map.keysSet (view boundTo sys)


isRole :: ComponentTypes -> System -> Name -> Bool
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
            Just (CompartmentType rTys) ->
                let cs = containedRoles name sys
                    cTys = fmap (`Map.lookup` (sys^.instances)) cs
                in sort cTys == sort (fmap Just rTys)
            _ -> True


data Model = Model ComponentTypes System deriving (Show)

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

        return (Model types sys)


genInstances :: ComponentTypes -> Gen (Map Name TypeName)
genInstances =
    fmap (Map.fromList . fmap mkInstance) . nonEmptySublistOf . Map.keys
  where
    mkInstance tyName = (mkInstanceName tyName, tyName)


genBindings :: ComponentTypes
            -> Map Name TypeName
            -> Gen (Map RoleName Name)
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
    compartmentType n =
        (,) n . CompartmentType <$> nonEmptySublistOf roleTyNames


typeNames :: Text -> Gen [TypeName]
typeNames name = do
    upper <- getNonNegative <$> arbitrary
    return (fmap (mkTypeName name) [0..upper])


mkTypeName :: Text -> Integer -> TypeName
mkTypeName n i = TypeName (appendIndex n i)


mkInstanceName :: TypeName -> Name
mkInstanceName = toLower . getTypeName


nonEmptySublistOf :: [a] -> Gen [a]
nonEmptySublistOf xs = sublistOf xs `suchThat` (not . null)
