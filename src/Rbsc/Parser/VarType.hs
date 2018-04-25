-- | Parser for variable types.
module Rbsc.Parser.VarType
    ( varType
    ) where


import Text.Megaparsec


import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


-- | Parser for variable types.
varType :: Parser UVarType
varType = label "type" . choice $
    [ VarTyBool  <$  reserved "bool"
    , VarTyInt   <$> range
    , VarTyArray <$> (reserved "array" *> range) <*> (reserved "of" *> varType)
    ]
