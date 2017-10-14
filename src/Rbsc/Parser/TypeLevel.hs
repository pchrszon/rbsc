-- | Parsers for user-defined natural types, role types and compartment types.
module Rbsc.Parser.TypeLevel
    ( declType
    ) where


import Text.Megaparsec

import Rbsc.Parser.Declaration
import Rbsc.Parser.Lexer
import Rbsc.Syntax.Declaration
import Rbsc.Syntax.TypeLevel


-- | Parser for a top-level type declaration.
declType :: ParserT m ErrorOrDecl
declType = withRecovery onSemi . fmap Right . choice $
    [ DeclNaturalType <$> naturalTypeDef
    , DeclRoleType <$> roleTypeDef
    , DeclCompartmentType <$> compartmentTypeDef
    ]


-- | Parser for a natural type definition.
naturalTypeDef :: ParserT m NaturalTypeDef
naturalTypeDef = NaturalTypeDef <$> (keyword *> identifier <* semi)
  where
    keyword = reserved "natural" *> reserved "type"


-- | Parser for a role type definition.
roleTypeDef :: ParserT m RoleTypeDef
roleTypeDef =
    RoleTypeDef <$> (keyword *> identifier) <*>
    (parens (identifier `sepBy` comma) <* semi)
  where
    keyword = reserved "role" *> reserved "type"


-- | Parser for a compartment type definition.
compartmentTypeDef :: ParserT m CompartmentTypeDef
compartmentTypeDef =
    CompartmentTypeDef <$> (keyword *> identifier) <*>
    (parens (identifier `sepBy` comma) <* semi)
  where
    keyword = reserved "compartment" *> reserved "type"


-- | Parser that consumes anything until a semicolon.
onSemi :: ParseError Char Dec -> ParserT m (Either (ParseError Char Dec) a)
onSemi err = Left err <$ anyChar `manyTill` semi
