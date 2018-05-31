-- | Parser for implementation definitions.
module Rbsc.Parser.Impl
    ( implementationDef
    , moduleDef
    ) where


import Text.Megaparsec


import Rbsc.Data.ComponentType

import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.VarDecl

import Rbsc.Syntax.Untyped


-- | Parser for an implementation definition.
implementationDef :: Parser Definition
implementationDef =
    DefImplementation <$> implementation <?> "implementation definition"


implementation :: Parser UImplementation
implementation = Implementation
    <$> (reserved "impl" *> identifier)
    <*> implementationBody
  where
    implementationBody =
        ImplModules <$> (parens (commaSepNonEmpty identifier) <* semi) <|>
        ImplSingle  <$> braces moduleBody


moduleDef :: Parser Definition
moduleDef = DefModule <$> modul <?> "module definition"


modul :: Parser UModule
modul = Module <$> (reserved "module" *> identifier) <*> braces moduleBody


moduleBody :: Parser UModuleBody
moduleBody = ModuleBody <$> many (varDecl <* semi) <*> elemMultis command many


command :: Parser UCommand
command = label "command" $ do
    (actKind, act) <- brackets $
        (,) <$> option NormalAction (OverrideAction <$> reserved "override")
            <*> optional expr
    Command act actKind <$> expr <*> (operator "->" *> updates)


updates :: Parser [UElemMulti UUpdate]
updates =
    try singleUpdate <|>
    elemMultis update (`sepBy1` operator "+") <* semi
  where
    singleUpdate = do
        as <- assignments <* semi
        return [ElemSingle (Update Nothing as)]


update :: Parser UUpdate
update = Update <$> (Just <$> (expr <* operator ":")) <*> assignments


assignments :: Parser [UElemMulti UAssignment]
assignments =
    [] <$ reserved "true" <|>
    elemMultis assignment (`sepBy1` operator "&")


assignment :: Parser UAssignment
assignment = label "assignment" . parens $ Assignment
    <$> identifier
    <*> many (brackets expr)
    <*> (symbol "'" *> equals *> expr)


-- | Parser for a list of 'ElemMulti'. @elemMultis p c@ parses elements using
-- the parser @p@. Elements are combined into a list using the parser
-- combinator @c@.
elemMultis ::
       Monad m
    => ParserT m a
    -> (ParserT m (UElemMulti a) -> ParserT m [UElemMulti a])
    -> ParserT m [UElemMulti a]
elemMultis p c = c elemMulti
  where
    elemMulti = choice
        [ ElemLoop   <$> loop
        , ElemIf     <$> (reserved "if" *> expr) <*> braces (elemMultis p c)
        , ElemSingle <$> p
        ]

    loop = label "forall" $ Loop
        <$> (reserved "forall" *> identifier)
        <*> option (QdTypeComponent AllComponents) (colon *> quantifiedType)
        <*> braces (elemMultis p c)
