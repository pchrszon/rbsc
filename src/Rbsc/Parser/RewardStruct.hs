{-# LANGUAGE OverloadedStrings #-}


-- | Parser for reward structures.
module Rbsc.Parser.RewardStruct
    ( rewardStructDef
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.Coordinator
import Rbsc.Parser.Impl

import Rbsc.Syntax.Untyped


-- | Parser for a reward structure definition.
rewardStructDef :: Parser Definition
rewardStructDef = DefRewardStruct <$> rewardStruct <?> "reward structure"


rewardStruct :: Parser URewardStruct
rewardStruct = RewardStruct
    <$> (reserved "rewards" *> optional stringLiteral)
    <*> braces (elemMultis (rewardStructItem <* semi) many)


rewardStructItem :: Parser URewardStructItem
rewardStructItem = label "reward item" $ RewardStructItem
    <$> rewardKind
    <*> expr
    <*> (operator ":=" *> expr)


rewardKind :: Parser URewardKind
rewardKind = option StateReward $
    TransitionReward
        <$> brackets (optional expr)
        <*> optional (brackets (playingConstraint Nothing))
