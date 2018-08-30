{-# LANGUAGE OverloadedStrings #-}


-- | Parser for label definitions.
module Rbsc.Parser.Label
    ( labelDef
    ) where


import Text.Megaparsec hiding (Label)


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


labelDef :: Parser Definition
labelDef = DefLabel <$> label' <?> "label definition"


label' :: Parser ULabel
label' = Label
    <$> (reserved "label" *> stringLiteral)
    <*> (equals *> expr <* semi)
