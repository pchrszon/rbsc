module Rbsc.Syntax.Constant
    ( ConstantType(..)
    , ConstantDef(..)
    ) where


import Rbsc.Name
import Rbsc.Report.Region
import Rbsc.Syntax.Expr.Untyped


-- | The type of a constant.
data ConstantType
    = TyBool
    | TyInt
    | TyDouble
    deriving (Show)


-- | A definition of a constant.
data ConstantDef = ConstantDef
    { constName :: Loc Name
    , constType :: !ConstantType
    , constExpr :: Loc Expr
    } deriving (Show)
