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
coordinator = do
    _ <- reserved "coordinator"
    roles <- option [] over

    braces $ Coordinator
        <$> many (varDecl <* semi)
        <*> elemMultis (coordCommand roles) many


coordCommand :: Monad m => [LExpr] -> ParserT m UCoordCommand
coordCommand roles = label "command" $ CoordCommand
    <$> brackets (optional expr)
    <*> optional (brackets (playingConstraint roles))
    <*> expr
    <*> (operator "->" *> updates)


playingConstraint :: Monad m => [LExpr] -> ParserT m UPlayingConstraint
playingConstraint roles = PlayingConstraint
    <$> expr
    <*> fmap (++ roles) (option [] over)


over :: Parser [LExpr]
over = reserved "over" *> (expr `sepBy1` comma)
