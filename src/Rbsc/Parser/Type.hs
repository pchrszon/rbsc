-- | Parser for types.
module Rbsc.Parser.Type
    ( typ
    ) where


import Text.Megaparsec


import Rbsc.Parser.Lexer

import Rbsc.Syntax.Type


-- | Parser for types.
typ :: Parser Type
typ = label "type" $ choice
    [ TyBool   <$ reserved "bool"
    , TyInt    <$ reserved "int"
    , TyDouble <$ reserved "double"
    , TyArray  <$> (reserved "array" *> reserved "of" *> typ)
    ]
