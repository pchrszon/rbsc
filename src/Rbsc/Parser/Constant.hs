{-# LANGUAGE OverloadedStrings #-}


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


constant :: Parser UConstant
constant =
    Constant <$>
    (reserved "const" *> identifier) <*>
    optional (colon *> typ) <*>
    (equals *> expr <* semi)
