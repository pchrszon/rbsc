{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


-- | Parsers for user-defined natural types, role types and compartment types.
module Rbsc.Parser.ComponentType
    ( componentTypeDef
    , typeSetDef
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Impl
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


-- | Parser for a component type definition. A type definition may
-- optionally provide the 'Implementation'.
componentTypeDef :: Parser [Definition]
componentTypeDef = label "definition" $ choice
    [ naturalTypeDef
    , roleTypeDef
    , compartmentTypeDef
    ]


naturalTypeDef :: Parser [Definition]
naturalTypeDef = typeDef
    (reserved "type" <|> (reserved "natural" *> reserved "type"))
    (return . DefNaturalType . NaturalTypeDef)


roleTypeDef :: Parser [Definition]
roleTypeDef = typeDef
    (reserved "role" *> reserved "type")
    (\typeName -> DefRoleType . RoleTypeDef typeName <$> players)
  where
    players :: Parser [Loc TypeName]
    players = parens (identifier `sepBy` comma) <?> "list of player types"


compartmentTypeDef :: Parser [Definition]
compartmentTypeDef = typeDef
    (reserved "compartment" *> reserved "type")
    (\typeName -> DefCompartmentType . CompartmentTypeDef typeName <$> roles)
  where
    roles :: Parser [[UMultiRole]]
    roles = parens (multiRoleList `sepBy` symbol "|")

    multiRoleList :: Parser [UMultiRole]
    multiRoleList = multiRole `sepBy` comma


typeDef :: Parser a -> (Loc TypeName -> Parser Definition) -> Parser [Definition]
typeDef keyword p = do
    typeName <- keyword *> identifier
    def <- p typeName
    (def :) <$> implDef typeName


implDef :: Loc TypeName -> Parser [Definition]
implDef typeName = choice
    [ []     <$  semi
    , (: []) <$> impl typeName
    ]


impl :: Loc TypeName -> Parser Definition
impl typeName = DefImplementation . Implementation typeName . ImplSingle <$>
    braces moduleBody


multiRole :: Parser UMultiRole
multiRole = MultiRole <$> identifier <*> optional cardinalities


cardinalities :: Parser (LExpr, LExpr)
cardinalities = brackets $ do
    lower <- expr
    optional (operator ".." *> expr) >>= \case
        Just upper -> return (lower, upper)
        Nothing    -> return (lower, lower)


-- | Parser for a type set definition.
typeSetDef :: Parser Definition
typeSetDef = DefTypeSet <$> typeSet <?> "definition"


typeSet :: Parser TypeSetDef
typeSet = TypeSetDef
    <$> (reserved "typedef" *> identifier)
    <*> (equals *> braces (commaSepNonEmpty identifier) <* semi)
