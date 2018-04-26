-- | Abstract syntax of variable declarations.
module Rbsc.Syntax.VarDecl
    ( VarDecl(..)
    ) where


import Data.Function
import Data.Ord


import Rbsc.Data.Name

import Rbsc.Report.Region

import Rbsc.Syntax.VarType


-- | A variable declaration.
data VarDecl expr = VarDecl
    { declName :: Loc Name
    , declType :: VarType expr
    , declInit :: Maybe expr
    } deriving (Show)

instance Eq (VarDecl expr) where
    (==) = (==) `on` declName

instance Ord (VarDecl expr) where
    compare = comparing declName
