-- | Parser for types.
module Rbsc.Parser.Type
    ( typ
    ) where


import Text.Megaparsec


import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


-- | Parser for types.
typ :: Parser UType
typ = label "type" $ choice
    [ TyBool   <$ reserved "bool"
    , TyInt    <$ reserved "int"
    , TyDouble <$ reserved "double"
    , TyArray  <$> (reserved "array" *> range) <*> (reserved "of" *> typ)
    ]
