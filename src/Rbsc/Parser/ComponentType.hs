{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


-- | Parsers for user-defined natural types, role types and compartment types.
module Rbsc.Parser.ComponentType
    ( componentTypeDef
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


-- | Parser for a componen type definition.
componentTypeDef :: Parser Definition
componentTypeDef = label "type definition" $ choice
    [ DefNaturalType <$> naturalTypeDef
    , DefRoleType <$> roleTypeDef
    , DefCompartmentType <$> compartmentTypeDef
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
compartmentTypeDef :: Parser UCompartmentTypeDef
compartmentTypeDef =
    CompartmentTypeDef <$> (keyword *> identifier) <*>
    (parens (multiRoleList `sepBy` symbol "|") <* semi)
  where
    keyword = reserved "compartment" *> reserved "type"
    multiRoleList = multiRole `sepBy` comma


multiRole :: Parser UMultiRole
multiRole = MultiRole <$> identifier <*> optional cardinalities


cardinalities :: Parser (LExpr, LExpr)
cardinalities = brackets $ do
    lower <- expr
    optional (operator ".." *> expr) >>= \case
        Just upper -> return (lower, upper)
        Nothing    -> return (lower, lower)
