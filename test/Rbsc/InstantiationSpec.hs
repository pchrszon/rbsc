{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}


module Rbsc.InstantiationSpec (spec) where


import Control.Lens hiding (elements)

import           Data.List (sort)
import           Data.Map  (Map)
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import           Data.Text (Text, toLower)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Rbsc.Instantiation
import Rbsc.Util


spec :: Spec
spec = describe "system instance completion" $ do
    prop "binds all roles" $ \(Model types sys) ->
        allOf (traverse._Right) (rolesAreBound types) (completeSystem types sys)
    prop "fills all compartments" $ \(Model types sys) ->
        allOf (traverse._Right)
              (compartmentsAreFilled types)
              (completeSystem types sys)


rolesAreBound :: Types -> System -> Bool
rolesAreBound types sys =
    Set.filter (isRole types sys) (Map.keysSet (view instances sys)) ==
    Map.keysSet (view boundTo sys)


isRole :: Types -> System -> InstanceName -> Bool
isRole types sys name = case view (instances.at name) sys of
    Just tyName -> case Map.lookup tyName types of
        Just (TypeRole _) -> True
        _                 -> False
    _ -> False


compartmentsAreFilled :: Types -> System -> Bool
compartmentsAreFilled types sys =
    all isFilled (view (instances . to Map.assocs) sys)
  where
    isFilled (name, tyName) =
        case Map.lookup tyName types of
            Just (TypeCompartment rTys) ->
                let cs = containedRoles name sys
                    cTys = fmap (`Map.lookup` (sys^.instances)) cs
                in sort cTys == sort (fmap Just rTys)
            _ -> True


data Model = Model Types System deriving (Show)

instance Arbitrary Model where
    arbitrary = do
        types <- genTypes
        is <- genInstances types
        bt <- genBindings types is

        let sys = System
                    { _instances = is
                    , _boundTo = bt
                    , _containedIn = Map.empty
                    }

        return (Model types sys)


genInstances :: Types -> Gen (Map InstanceName TypeName)
genInstances =
    fmap (Map.fromList . fmap mkInstance) . nonEmptySublistOf . Map.keys
  where
    mkInstance tyName = (mkInstanceName tyName, tyName)


genBindings :: Types
            -> Map InstanceName TypeName
            -> Gen (Map RoleInstanceName InstanceName)
genBindings types is = do
    is' <- sublistOf (Map.assocs is)
    Map.unions <$> traverse (uncurry genBinding) is'
  where
    genBinding name tyName =
        case Map.lookup tyName types of
            Just (TypeRole playerTyNames) -> do
                let playerNames =
                        Set.fromList (fmap mkInstanceName playerTyNames)
                    possiblePlayers =
                        Set.intersection (Map.keysSet is) playerNames
                if null possiblePlayers
                    then return Map.empty
                    else Map.singleton name <$>
                         elements (Set.toList possiblePlayers)
            _ -> return Map.empty


genTypes :: Gen Types
genTypes = scale (min 1) $ do
    componentTyNames <- typeNames "A"
    roleTyNames <- typeNames "R"
    compartmentTyNames <- typeNames "C"

    -- intentionally left out roles as players since instantiation often
    -- leads to exponential blow-up
    let playerTyNames = componentTyNames ++ compartmentTyNames

    atys <- componentTypes componentTyNames
    rtys <- compositeTypes TypeRole roleTyNames playerTyNames
    ctys <- compositeTypes TypeCompartment compartmentTyNames roleTyNames

    return (Map.unions [atys, rtys, ctys])


componentTypes :: [TypeName] -> Gen Types
componentTypes = pure . Map.fromList . fmap (, TypeComponent)


compositeTypes :: ([TypeName] -> Type) -> [TypeName] -> [TypeName] -> Gen Types
compositeTypes tyCon tyNames innerTyNames =
    Map.fromList <$> traverse compositeType tyNames
  where
    compositeType n = (,) n . tyCon <$> nonEmptySublistOf innerTyNames


typeNames :: Text -> Gen [TypeName]
typeNames name = do
    upper <- getNonNegative <$> arbitrary
    return (fmap (mkTypeName name) [0..upper])


mkTypeName :: Text -> Integer -> TypeName
mkTypeName n i = TypeName (appendIndex n i)


mkInstanceName :: TypeName -> InstanceName
mkInstanceName = InstanceName . toLower . getTypeName


nonEmptySublistOf :: [a] -> Gen [a]
nonEmptySublistOf xs = sublistOf xs `suchThat` (not . null)
