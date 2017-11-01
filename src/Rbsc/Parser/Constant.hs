-- | Parser for constant definitions.
module Rbsc.Parser.Constant
    ( constantDef
    ) where


import Text.Megaparsec

import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Constant


-- | Parser for a constant definition.
constantDef :: Parser Definition
constantDef = DefConstant <$> constant


constant :: Parser ConstantDef
constant =
    ConstantDef <$>
    (reserved "const" *> identifier) <*>
    (colon *> constantType) <*>
    (equals *> expr <* semi)


constantType :: Parser ConstantType
constantType = choice
    [ TyBool   <$ reserved "bool"
    , TyInt    <$ reserved "int"
    , TyDouble <$ reserved "double"
    ]
