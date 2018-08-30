-- | Abstract syntax of label definitions.
module Rbsc.Syntax.Label
    ( Label(..)
    ) where


import Rbsc.Data.Name

import Rbsc.Report.Region


-- | A label definition.
data Label expr = Label
    { labelName :: Loc Name
    , labelExpr :: expr
    } deriving (Show)
