{-# LANGUAGE OverloadedStrings #-}

-- | Parser for coordinators.
module Rbsc.Parser.Coordinator
    ( coordinatorDef
    , playingConstraint
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
    roles <- optional over

    braces $ Coordinator
        <$> many (varDecl <* semi)
        <*> elemMultis (coordCommand roles) many


coordCommand :: Monad m => Maybe LExpr -> ParserT m UCoordCommand
coordCommand roles = label "command" $ CoordCommand
    <$> brackets (optional expr)
    <*> optional (brackets (playingConstraint roles))
    <*> expr
    <*> (operator "->" *> updates)


playingConstraint :: Monad m => Maybe LExpr -> ParserT m UPlayingConstraint
playingConstraint roles = PlayingConstraint
    <$> expr
    <*> option roles (Just <$> over)


over :: Parser LExpr
over = reserved "over" *> expr
