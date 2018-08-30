{-# LANGUAGE OverloadedStrings #-}


-- | Parser for enumerations.
module Rbsc.Parser.Enumeration
    ( enumerationDef
    , enumeration
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


enumerationDef :: Parser Definition
enumerationDef = DefEnumeration <$> enumeration <?> "enumeration"


-- | Parser for enumerations.
enumeration :: Parser Enumeration
enumeration =
    Enumeration <$> (reserved "enum" *> braces (identifier `sepBy1` comma))
