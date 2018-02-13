{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Built-in functions.
module Rbsc.Data.Function
    ( Fn(..)

    , FunctionName(..)
    , TypedFunction(..)
    , function
    ) where


-- | Wrapper for a function.
--
-- This wrapper provides dummy instances for 'Show' which (for obvious reasons)
-- does not exist for the type @a -> b@.
newtype Fn a = Fn { getFn :: a }

instance Show (Fn a) where
    show _ = "<function>"


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
    MinInt    :: TypedFunction (Integer -> Fn (Integer -> Integer))
    MinDouble :: TypedFunction (Double -> Fn (Double -> Double))
    MaxInt    :: TypedFunction (Integer -> Fn (Integer -> Integer))
    MaxDouble :: TypedFunction (Double -> Fn (Double -> Double))
    Floor     :: TypedFunction (Double -> Integer)
    Ceil      :: TypedFunction (Double -> Integer)
    PowInt    :: TypedFunction (Integer -> Fn (Integer -> Integer))
    PowDouble :: TypedFunction (Double -> Fn (Double -> Double))
    Mod       :: TypedFunction (Integer -> Fn (Integer -> Integer))
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
