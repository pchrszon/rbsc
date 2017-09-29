{-# LANGUAGE DeriveFunctor #-}


module Rbsc.Syntax.Constraint where


import Rbsc.Syntax.Operators
import Rbsc.Type


-- | A constraint expression.
data Constraint l
    = LitBool !Bool !l
    | Variable !Name !l
    | Not (Constraint l) !l
    | BoolBinOp !BoolBinOp (Constraint l) (Constraint l) !l
    | HasType (Constraint l) !TypeName !l
    | BoundTo (Constraint l) (Constraint l) !l
    | Element (Constraint l) (Constraint l) !l
    | Quantifier !Name (Maybe TypeName) (Constraint l) !l
    deriving (Functor, Show)
