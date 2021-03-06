{-# LANGUAGE OverloadedStrings #-}


-- | The list of reserved keywords used to parse valid identifiers.
module Rbsc.Parser.Reserved
    ( reservedWords
    , opLetter
    ) where


import Data.Text (Text)


-- | List of reserved keywords.
reservedWords :: [Text]
reservedWords =
    [ "include"
    , "bool"
    , "int"
    , "enum"
    , "double"
    , "action"
    , "array"
    , "of"
    , "const"
    , "function"
    , "global"
    , "init"
    , "label"
    , "component"
    , "natural"
    , "role"
    , "compartment"
    , "type"
    , "typedef"
    , "true"
    , "false"
    , "forall"
    , "exists"
    , "sum"
    , "product"
    , "if"
    , "then"
    , "else"
    , "boundto"
    , "in"
    , "system"
    , "impl"
    , "module"
    , "coordinator"
    , "rewards"
    , "self"
    , "player"
    , "playable"
    , "index"
    , "min"
    , "minf"
    , "max"
    , "maxf"
    , "floor"
    , "ceil"
    , "pow"
    , "powf"
    , "mod"
    , "log"
    , "count"
    , "length"
    , "player"
    , "override"
    , "internal"
    , "over"
    , "constraint"
    , "observe"
    ]


-- | List of tail characters of operators.
--
-- This list is necessary to check whether a parsed operator is not
-- a prefix of another legal operator.
opLetter :: String
opLetter = "=>.|"
