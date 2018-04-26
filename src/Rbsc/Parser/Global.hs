-- | Parser for global definitions.
module Rbsc.Parser.Global
    ( globalDef
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Lexer
import Rbsc.Parser.VarDecl

import Rbsc.Syntax.Untyped


-- | Parser for a global definition.
globalDef :: Parser Definition
globalDef = DefGlobal . Global
    <$> (reserved "global" *> varDecl <* semi) <?> "global variable"
