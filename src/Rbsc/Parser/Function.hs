{-# LANGUAGE OverloadedStrings #-}


-- | Parser for function definitions.
module Rbsc.Parser.Function
    ( functionDef
    , parameter
    ) where


import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.Type

import Rbsc.Syntax.Untyped


-- | Parser for a function definition.
functionDef :: Parser Definition
functionDef = DefFunction <$> function <?> "definition"


function :: Parser UFunction
function = do
    _ <- reserved "function"
    fstIdent <- identifier
    mSndIdent <- optional (dot *> identifier)

    let (mTyName, name) = case mSndIdent of
            Just sndIdent -> (Just (fmap TypeName fstIdent), sndIdent)
            Nothing       -> (Nothing, fstIdent)

    Function mTyName name
        <$> parens (parameter `sepBy` comma)
        <*> (colon *> typ)
        <*> (equals *> expr <* semi)


parameter :: Parser UParameter
parameter = Parameter <$> identifier <*> (colon *> typ)
