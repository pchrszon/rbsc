-- | Parser for system definitions.
module Rbsc.Parser.System
    ( systemDef
    ) where


import Text.Megaparsec


import Rbsc.Syntax.Expr.Untyped (clauses)

import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer


-- | Parser for a system definition.
systemDef :: Parser Definition
systemDef =
    DefSystem <$>
    block "system" (concatMap clauses <$> expr `sepBy` comma) <?>
    "system definition"
