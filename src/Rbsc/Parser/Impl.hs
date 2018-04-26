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
moduleBody = ModuleBody <$> many (varDecl <* semi) <*> body command many


command :: Parser UCommand
command = label "command" $ Command
    <$> brackets (optional expr)
    <*> expr
    <*> (operator "->" *> updates)


updates :: Parser (UBody (Update ComponentTypeSet))
updates =
    try singleUpdate <|>
    body update (`sepBy1` operator "+") <* semi
  where
    singleUpdate = do
        as <- assignments <* semi
        return (Body [ItemSingle (Update Nothing as)])


update :: Parser UUpdate
update = Update <$> (Just <$> (expr <* operator ":")) <*> assignments


assignments :: Parser (UBody Assignment)
assignments =
    Body [] <$ reserved "true" <|>
    body assignment (`sepBy1` operator "&")


assignment :: Parser UAssignment
assignment = label "assignment" . parens $ Assignment
    <$> identifier
    <*> many (brackets expr)
    <*> (symbol "'" *> equals *> expr)


-- | Parser for a 'Body'. @body p c@ parses items using the parser @p@.
-- Items are combined into a list using the parser combinator @c@.
body ::
       Monad m
    => ParserT m (a (Loc Expr))
    -> (ParserT m (UBodyItem a) -> ParserT m [UBodyItem a])
    -> ParserT m (UBody a)
body p c = Body <$> c bodyItem
  where
    bodyItem = choice
        [ ItemLoop   <$> loop (body p c)
        , ItemIf     <$> (reserved "if" *> expr) <*> braces (body p c)
        , ItemSingle <$> p
        ]


loop :: Monad m => ParserT m (a LExpr) -> ParserT m (ULoop a)
loop p = label "forall" $ Loop
    <$> (reserved "forall" *> identifier)
    <*> option (QdTypeComponent AllComponents) (colon *> quantifiedType)
    <*> braces p
