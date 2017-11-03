{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}


-- | Built-in functions.
module Rbsc.Data.Function
    ( Fn(..)

    , FunctionSym(..)
    , Function(..)
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
data FunctionSym
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
data Function t where
    MinInt    :: Function (Integer -> Fn (Integer -> Integer))
    MinDouble :: Function (Double -> Fn (Double -> Double))
    MaxInt    :: Function (Integer -> Fn (Integer -> Integer))
    MaxDouble :: Function (Double -> Fn (Double -> Double))
    Floor     :: Function (Double -> Integer)
    Ceil      :: Function (Double -> Integer)
    PowInt    :: Function (Integer -> Fn (Integer -> Integer))
    PowDouble :: Function (Double -> Fn (Double -> Double))
    Mod       :: Function (Integer -> Fn (Integer -> Integer))
    Log       :: Function (Double -> Fn (Double -> Double))

deriving instance Eq (Function t)
deriving instance Show (Function t)


-- | Semantics of a built-in function.
function :: Function t -> t
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
