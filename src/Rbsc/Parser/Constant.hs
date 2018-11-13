{-# LANGUAGE OverloadedStrings #-}


-- | Parser for constant definitions.
module Rbsc.Parser.Constant
    ( constantDef
    ) where


import Control.Lens

import Data.Text (unpack)

import Text.Megaparsec


import Rbsc.Parser.Definition
import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer
import Rbsc.Parser.Type

import Rbsc.Syntax.Untyped


-- | Parser for a constant definition.
constantDef :: Parser Definition
constantDef = DefConstant <$> constant <?> "constant definition"


constant :: Parser UConstant
constant = do
    name <- reserved "const" *> identifier
    mTy  <- optional (colon *> typ)
    mDef <- optional (equals *> expr)

    mArgDef <- use (constArgs.at (unLoc name))

    case mArgDef <|> mDef of
        Just def -> Constant name mTy def <$ semi
        Nothing  -> do
            let name' = unpack (unLoc name)
            fail $
                "value of " ++ name' ++ " is undefined" ++
                "\neither insert a definition here" ++
                "\nor provide a value on the command line using --const " ++
                name' ++ "=<VALUE>"
