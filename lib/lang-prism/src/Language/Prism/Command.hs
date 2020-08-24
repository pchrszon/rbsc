{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


-- | Abstract syntax of commands.
module Language.Prism.Command
    ( Command(..)
    , Action(..)
    , ActionType(..)
    , Update(..)

    , prettyAction
    ) where


import Data.Text.Prettyprint.Doc

import Language.Prism.Expression


-- | A guarded command.
data Command = Command
    { cmdActions    :: [Action]
    , cmdActionType :: !ActionType
    , cmdGuard      :: !Expr
    , cmdUpdates    :: [Update]
    } deriving (Show)

instance Pretty Command where
    pretty Command {..} =
        prettyAction cmdActions cmdActionType <+>
        pretty cmdGuard <+>
        "->" <> nest 4 (softline <> prettyList cmdUpdates <> semi)


prettyAction :: [Action] -> ActionType -> Doc ann
prettyAction acts ty =
    actionType ty (fillSep (punctuate comma (fmap pretty acts)))


data Action
    = Action !Ident
    | Tau !Int
    deriving (Eq, Ord, Show)

instance Pretty Action where
    pretty = \case
        Action act -> pretty act
        Tau _      -> emptyDoc


-- | An action can either be open or closed.
data ActionType
    = ActionClosed
    | ActionOpen
    deriving (Eq, Ord, Show)

actionType :: ActionType -> Doc ann -> Doc ann
actionType at = case at of
    ActionClosed -> brackets
    ActionOpen   -> enclose rbracket lbracket


-- | A stochastic update.
data Update = Update
    { updProb        :: !(Maybe Expr)
    , updAssignments :: [(Ident, Expr)]
    } deriving (Show)

instance Pretty Update where
    pretty Update{..} =
        maybe mempty ((<> colon) . pretty) updProb <>
        prettyAssignments updAssignments

    prettyList [] = "true"
    prettyList us = fillSep (punctuate " +" (fmap pretty us))


prettyAssignments :: [(Ident, Expr)] -> Doc ann
prettyAssignments [] = "true"
prettyAssignments as = fillSep (punctuate " &" (fmap prettyAssignment as))


prettyAssignment :: (Ident, Expr) -> Doc ann
prettyAssignment (ident, e) = parens (pretty ident <> "'" <+> "=" <+> pretty e)
