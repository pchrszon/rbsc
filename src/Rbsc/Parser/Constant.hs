-- | Parser for constant definitions.
module Rbsc.Parser.Constant
    ( declConstant
    ) where


import Text.Megaparsec

import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Constant
import Rbsc.Syntax.Declaration


-- | Parser for a constant definition.
declConstant :: Parser Declaration
declConstant = DeclConstant <$> constantDef


constantDef :: Parser ConstantDef
constantDef =
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
