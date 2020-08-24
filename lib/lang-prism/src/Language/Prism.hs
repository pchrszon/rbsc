{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Abstract syntax of PRISM models and property lists.
module Language.Prism
    ( Model(..)
    , ModelType(..)

    , PropertyList(..)
    , Property(..)

    , Formula(..)

    , Label(..)

    , Constant(..)
    , ConstantType(..)

    , GlobalVar(..)

    , Module(..)

    , RewardStruct(..)
    , RewardStructItem(..)
    , RewardKind(..)

    , module Export
    ) where


import Data.Text.Prettyprint.Doc

import Language.Prism.Command     as Export
import Language.Prism.Declaration as Export
import Language.Prism.Expression  as Export
import Language.Prism.Functions   as Export
import Language.Prism.Operators   as Export


-- | A PRISM model.
data Model = Model
    { modelType          :: !ModelType
    , modelFormulas      :: [Formula]
    , modelLabels        :: [Label]
    , modelConstants     :: [Constant]
    , modelGlobalVars    :: [GlobalVar]
    , modelModules       :: [Module]
    , modelInitStates    :: Maybe Expr
    , modelRewardStructs :: [RewardStruct]
    } deriving (Show)

instance Pretty Model where
    pretty Model{..} = blocks
        [ [pretty modelType]
        , pretty <$> modelFormulas
        , pretty <$> modelLabels
        , pretty <$> modelConstants
        , pretty <$> modelGlobalVars
        , punctuate hardline (fmap pretty modelModules)
        , maybe [] ((:[]) . prettyInits) modelInitStates
        , punctuate hardline (fmap pretty modelRewardStructs)
        ] <> line -- make sure last line is newline terminated


-- | The type of a PRISM model.
data ModelType
    = MDP
    | DTMC
    | CTMC
    deriving (Show)

instance Pretty ModelType where
    pretty = \case
        MDP  -> "mdp"
        DTMC -> "dtmc"
        CTMC -> "ctmc"


-- | A PRISM property list.
data PropertyList = PropertyList
    { plFormulas   :: [Formula]
    , plLabels     :: [Label]
    , plConstants  :: [Constant]
    , plProperties :: [Property]
    } deriving (Show)

instance Pretty PropertyList where
    pretty PropertyList{..} = blocks
        [ pretty <$> plFormulas
        , pretty <$> plLabels
        , pretty <$> plConstants
        , pretty <$> plProperties
        ]


-- | A property.
data Property = Property
    { propName :: !(Maybe Ident)
    , propExpr :: !Expr
    } deriving (Show)

instance Pretty Property where
    pretty Property{..} =
        maybe mempty ((<> ": ") . dquotes . pretty) propName <>
        pretty propExpr <> semi


-- | A formula definition.
data Formula = Formula
    { formName :: !Ident
    , formExpr :: !Expr
    } deriving (Show)

instance Pretty Formula where
    pretty Formula{..} =
        "formula" <+> pretty formName <+> "=" <+> pretty formExpr <> semi


-- | A label definition.
data Label = Label
    { lblName :: !Ident
    , lblExpr :: !Expr
    } deriving (Show)

instance Pretty Label where
    pretty Label{..} =
        "label" <+> dquotes (pretty lblName) <+> "=" <+> pretty lblExpr <> semi


-- | A constant definition.
data Constant = Constant
    { constName :: !Ident
    , constType :: !ConstantType
    , constInit :: !(Maybe Expr)
    } deriving (Show)

instance Pretty Constant where
    pretty Constant{..} =
        "const" <+> pretty constType <+> pretty constName <>
        maybe mempty ((" = " <>) . pretty) constInit <> semi


-- | The type of a constant.
data ConstantType
    = ConstTypeBool
    | ConstTypeInt
    | ConstTypeDouble
    deriving (Show)

instance Pretty ConstantType where
    pretty = \case
        ConstTypeBool   -> "bool"
        ConstTypeInt    -> "int"
        ConstTypeDouble -> "double"


-- | A declaration of a global variable.
newtype GlobalVar = GlobalVar Declaration deriving (Show)

instance Pretty GlobalVar where
    pretty (GlobalVar decl) = "global" <+> pretty decl


-- | A module.
data Module = Module
    { modIdent     :: !Ident
    , modVariables :: [Declaration]
    , modCommands  :: [Command]
    } deriving (Show)

instance Pretty Module where
    pretty Module{..} = "module" <+> pretty modIdent <> line <>
        body <> line <>
        "endmodule"
      where
        body =
            (if null modVariables
                then mempty
                else indent 4 (vcat (fmap pretty modVariables))) <>
            (if null modVariables || null modCommands
                then mempty
                else hardline <> hardline) <>
            (if null modCommands
                then mempty
                else indent 4 (vcat (fmap pretty modCommands)))


-- | A reward structure.
data RewardStruct = RewardStruct
    { rsName  :: !(Maybe Ident)
    , rsItems :: [RewardStructItem]
    } deriving (Show)

instance Pretty RewardStruct where
    pretty RewardStruct{..} =
        "rewards" <> maybe mempty ((space <>) . dquotes . pretty) rsName <>
        line <> indent 4 (vcat (fmap pretty rsItems)) <> line <>
        "endrewards"


-- | An item in a reward structure.
data RewardStructItem = RewardStructItem
    { riKind   :: RewardKind
    , riGuard  :: !Expr
    , riReward :: !Expr
    } deriving (Show)

instance Pretty RewardStructItem where
    pretty RewardStructItem{..} =
        pretty riKind <>
        pretty riGuard <+> colon <+> pretty riReward <> semi


data RewardKind
    = TransitionReward [Action] !ActionType
    | StateReward
    deriving (Show)

instance Pretty RewardKind where
    pretty = \case
        TransitionReward acts ty -> prettyAction acts ty <> space
        StateReward              -> mempty


-- | @blocks xs@ vertically concatenates the documents @xs@ with an empty
-- line between them.
blocks :: [[Doc ann]] -> Doc ann
blocks = vcat . punctuate hardline . fmap vcat . filter (not . null)


-- | Prints the expression as @init@ block.
prettyInits :: Expr -> Doc ann
prettyInits e = "init" <> line <> indent 4 (pretty e) <> line <> "endinit"
