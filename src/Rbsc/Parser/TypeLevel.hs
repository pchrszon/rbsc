{-# LANGUAGE RankNTypes #-}


-- | Parsers for user-defined natural types, role types and compartment types.
module Rbsc.Parser.TypeLevel
    ( declType
    ) where


import Text.Megaparsec

import Rbsc.Parser.Lexer
import Rbsc.Syntax.Declaration
import Rbsc.Syntax.TypeLevel


-- | Parser for a top-level type declaration.
declType :: Parser Declaration
declType = choice
    [ DeclNaturalType <$> naturalTypeDef
    , DeclRoleType <$> roleTypeDef
    , DeclCompartmentType <$> compartmentTypeDef
    ]


-- | Parser for a natural type definition.
naturalTypeDef :: Parser NaturalTypeDef
naturalTypeDef = NaturalTypeDef <$> (keyword *> identifier <* semi)
  where
    keyword = reserved "natural" *> reserved "type"


-- | Parser for a role type definition.
roleTypeDef :: Parser RoleTypeDef
roleTypeDef =
    RoleTypeDef <$> (keyword *> identifier) <*>
    (parens (identifier `sepBy` comma) <* semi)
  where
    keyword = reserved "role" *> reserved "type"


-- | Parser for a compartment type definition.
compartmentTypeDef :: Parser CompartmentTypeDef
compartmentTypeDef =
    CompartmentTypeDef <$> (keyword *> identifier) <*>
    (parens (identifier `sepBy` comma) <* semi)
  where
    keyword = reserved "compartment" *> reserved "type"
