module Rbsc.Parser.System
    ( declSystem
    ) where


import Text.Megaparsec

import Rbsc.Parser.Constraint
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Declaration


-- | Parser for a system declaration.
declSystem :: Parser Declaration
declSystem = DeclSystem <$> block "system" (constraint `sepBy` comma)
