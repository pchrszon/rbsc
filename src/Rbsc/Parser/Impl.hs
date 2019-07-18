{-# LANGUAGE OverloadedStrings #-}


-- | Parser for implementation definitions.
module Rbsc.Parser.Impl
    ( implementationDef
    , moduleDef
    , moduleBody
    , updates
    , elemMultis
    ) where


import Text.Megaparsec


import Rbsc.Data.Action
import Rbsc.Data.ComponentType

import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Function
import Rbsc.Parser.Lexer
import Rbsc.Parser.VarDecl

import Rbsc.Syntax.Untyped


-- | Parser for an implementation definition.
implementationDef :: Parser Definition
implementationDef =
    DefImplementation <$> implementation <?> "definition"


implementation :: Parser UImplementation
implementation = Implementation
    <$> (reserved "impl" *> identifier)
    <*> implementationBody
  where
    implementationBody =
        ImplModules <$> (parens (commaSepNonEmpty moduleRef) <* semi) <|>
        ImplSingle  <$> braces moduleBody


moduleRef :: Parser UModuleRef
moduleRef = ModuleRef
    <$> identifier
    <*> option [] (parens (expr `sepBy` comma))


moduleDef :: Parser Definition
moduleDef = DefModule <$> modul <?> "definition"


modul :: Parser UModule
modul = Module
    <$> (reserved "module" *> identifier)
    <*> option [] (parens (parameter `sepBy` comma))
    <*> braces moduleBody


moduleBody :: Parser UModuleBody
moduleBody = ModuleBody <$> many (varDecl <* semi) <*> elemMultis command many


command :: Parser UCommand
command = label "command" $ do
    ((actKind, actIntent), act) <- brackets . option (noModifier, Nothing) $
        (,) <$> modifier <*> (Just <$> (expr <?> "action"))
    Command act actKind actIntent <$> expr <*> (operator "->" *> updates)
  where
    modifier = option noModifier $ choice
        [ (,) <$>
            (OverrideAction <$> reserved "override") <*>
            pure ExternalAction
        , (NormalAction, InternalAction) <$ reserved "internal"
        ]
    noModifier = (NormalAction, ExternalAction)


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

    loop = do
        l <- Loop
            <$> (reserved "forall" *> identifier)
            <*> option (QdTypeComponent AllComponents) (colon *> quantifiedType)

        mGuard <- optional (dot *> expr)
        body   <- braces (elemMultis p c)

        return $ case mGuard of
            Just g  -> l [ElemIf g body]
            Nothing -> l body
