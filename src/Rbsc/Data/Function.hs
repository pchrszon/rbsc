{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Built-in functions.
module Rbsc.Data.Function
    ( FunctionName(..)
    , TypedFunction(..)
    , function
    , functionType
    ) where


import Rbsc.Data.Type


-- | Abstract syntax for built-in functions.
data FunctionName
    = FuncMinInt
    | FuncMinDouble
    | FuncMaxInt
    | FuncMaxDouble
    | FuncFloor
    | FuncCeil
    | FuncPowInt
    | FuncPowDouble
    | FuncMod
    | FuncLog
    deriving (Eq, Show)


-- | Typed abstract syntax for built-in functions.
data TypedFunction t where
    MinInt    :: TypedFunction (Int -> Fn (Int -> Int))
    MinDouble :: TypedFunction (Double -> Fn (Double -> Double))
    MaxInt    :: TypedFunction (Int -> Fn (Int -> Int))
    MaxDouble :: TypedFunction (Double -> Fn (Double -> Double))
    Floor     :: TypedFunction (Double -> Int)
    Ceil      :: TypedFunction (Double -> Int)
    PowInt    :: TypedFunction (Int -> Fn (Int -> Int))
    PowDouble :: TypedFunction (Double -> Fn (Double -> Double))
    Mod       :: TypedFunction (Int -> Fn (Int -> Int))
    Log       :: TypedFunction (Double -> Fn (Double -> Double))

deriving instance Eq (TypedFunction t)
deriving instance Show (TypedFunction t)


-- | Semantics of a built-in function.
function :: TypedFunction t -> t
function = \case
    MinInt    -> Fn . min
    MinDouble -> Fn . min
    MaxInt    -> Fn . max
    MaxDouble -> Fn . max
    Floor     -> floor
    Ceil      -> ceiling
    PowInt    -> Fn . (^)
    PowDouble -> Fn . (**)
    Mod       -> Fn . mod
    Log       -> Fn . logBase


-- | The 'Type' of a built-in function.
functionType :: TypedFunction t -> Type (Fn t)
functionType = \case
    MinInt    -> TyFunc TyInt (TyFunc TyInt TyInt)
    MinDouble -> TyFunc TyDouble (TyFunc TyDouble TyDouble)
    MaxInt    -> TyFunc TyInt (TyFunc TyInt TyInt)
    MaxDouble -> TyFunc TyDouble (TyFunc TyDouble TyDouble)
    Floor     -> TyFunc TyDouble TyInt
    Ceil      -> TyFunc TyDouble TyInt
    PowInt    -> TyFunc TyInt (TyFunc TyInt TyInt)
    PowDouble -> TyFunc TyDouble (TyFunc TyDouble TyDouble)
    Mod       -> TyFunc TyInt (TyFunc TyInt TyInt)
    Log       -> TyFunc TyDouble (TyFunc TyDouble TyDouble)
