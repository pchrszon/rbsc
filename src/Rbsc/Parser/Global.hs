-- | Parser for global definitions.
module Rbsc.Parser.Global
    ( globalDef
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.VarType

import Rbsc.Syntax.Untyped


-- | Parser for a global definition.
globalDef :: Parser Definition
globalDef = DefGlobal <$> global <?> "global variable"


global :: Parser UGlobal
global = Global
    <$> (reserved "global" *> identifier)
    <*> (colon *> varType)
    <*> optional (reserved "init" *> expr) <* semi
