module Rbsc.Parser.VarDecl
    ( varDecl
    ) where


import Text.Megaparsec


import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.VarType

import Rbsc.Syntax.Untyped


varDecl :: Parser UVarDecl
varDecl = VarDecl
    <$> identifier
    <*> (colon *> varType)
    <*> optional (reserved "init" *> expr)
