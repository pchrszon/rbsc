{-# LANGUAGE LambdaCase #-}


-- | Parser for types.
module Rbsc.Parser.Type
    ( typ
    ) where


import Text.Megaparsec


import Rbsc.Parser.Expr
import Rbsc.Parser.Lexer

import Rbsc.Syntax.Untyped


-- | Parser for types.
typ :: Parser UType
typ = label "type" $ do
    ty <- valueType
    optional (operator "->" *> typ) >>= return . \case
        Just tyRes -> TyFunc ty tyRes
        Nothing    -> ty


valueType :: Parser UType
valueType = choice
    [ TyBool      <$  reserved "bool"
    , TyInt       <$  reserved "int"
    , TyDouble    <$  reserved "double"
    , TyComponent <$> componentTypeSet
    , TyArray     <$> (reserved "array" *> range) <*> (reserved "of" *> typ)
    , parens typ
    ]
