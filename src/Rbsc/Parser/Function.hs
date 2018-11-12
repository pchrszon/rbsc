{-# LANGUAGE OverloadedStrings #-}


-- | Parser for function definitions.
module Rbsc.Parser.Function
    ( functionDef
    , parameter
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.Type

import Rbsc.Syntax.Untyped


-- | Parser for a function definition.
functionDef :: Parser Definition
functionDef = DefFunction <$> function <?> "function definition"


function :: Parser UFunction
function = Function
    <$> (reserved "function" *> identifier)
    <*> parens (parameter `sepBy` comma)
    <*> (colon *> typ)
    <*> (equals *> expr <* semi)


parameter :: Parser UParameter
parameter = Parameter <$> identifier <*> (colon *> typ)
