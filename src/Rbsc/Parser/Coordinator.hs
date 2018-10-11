{-# LANGUAGE OverloadedStrings #-}

-- | Parser for coordinators.
module Rbsc.Parser.Coordinator
    ( coordinatorDef
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Impl
import Rbsc.Parser.Lexer
import Rbsc.Parser.VarDecl

import Rbsc.Syntax.Untyped


-- | Parser for a coordinator definition.
coordinatorDef :: Parser Definition
coordinatorDef = DefCoordinator <$> coordinator <?> "coordinator definition"


coordinator :: Parser UCoordinator
coordinator = reserved "coordinator" *> braces (Coordinator
    <$> many (varDecl <* semi)
    <*> elemMultis coordCommand many)


coordCommand :: Parser UCoordCommand
coordCommand = label "command" $ CoordCommand
    <$> brackets (optional expr)
    <*> optional (brackets playingConstraint)
    <*> expr
    <*> (operator "->" *> updates)


playingConstraint :: Parser UPlayingConstraint
playingConstraint = PlayingConstraint
    <$> expr
    <*> option [] (reserved "over" *> (expr `sepBy1` comma))
