{-# LANGUAGE OverloadedStrings #-}


-- | Parser for observe definitions.
module Rbsc.Parser.Observe
    ( observeDef
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


-- | Parser for an observe definition.
observeDef :: Parser Definition
observeDef = DefObserve <$> observe <?> "observe definition"


observe :: Parser UObserve
observe = Observe <$> (reserved "observe" *> commaSepNonEmpty expr <* semi)
