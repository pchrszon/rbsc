-- | Parser for function definitions.
module Rbsc.Parser.Function
    ( functionDef
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
    <*> parens (commaSepNonEmpty parameter)
    <*> (colon *> typ)
    <*> (equals *> expr <* semi)


parameter :: Parser Parameter
parameter = Parameter <$> identifier <*> (colon *> typ)
