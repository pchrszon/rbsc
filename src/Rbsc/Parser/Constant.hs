-- | Parser for constant definitions.
module Rbsc.Parser.Constant
    ( constantDef
    ) where


import Text.Megaparsec

import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.Type

import Rbsc.Syntax.Untyped


-- | Parser for a constant definition.
constantDef :: Parser Definition
constantDef = DefConstant <$> constant <?> "constant definition"


constant :: Parser UConstantDef
constant =
    ConstantDef <$>
    (reserved "const" *> identifier) <*>
    (colon *> typ) <*>
    (equals *> expr <* semi)
