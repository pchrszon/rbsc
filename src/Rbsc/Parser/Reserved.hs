-- | The list of reserved keywords used to parse valid identifiers.
module Rbsc.Parser.Reserved
    ( reservedWords
    , opLetter
    ) where


-- | List of reserved keywords.
reservedWords :: [String]
reservedWords =
    [ "include"
    , "bool"
    , "int"
    , "double"
    , "array"
    , "of"
    , "const"
    , "function"
    , "global"
    , "init"
    , "component"
    , "natural"
    , "role"
    , "compartment"
    , "type"
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
    ]


-- | List of tail characters of operators.
--
-- This list is necessary to check whether a parsed operator is not
-- a prefix of another legal operator.
opLetter :: String
opLetter = "=>."
