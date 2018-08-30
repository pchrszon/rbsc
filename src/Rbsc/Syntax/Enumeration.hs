-- | Abstract syntax of enumerations.
module Rbsc.Syntax.Enumeration
    ( Enumeration(..)
    ) where


import Rbsc.Data.Name

import Rbsc.Report.Region


newtype Enumeration = Enumeration [Loc Name] deriving (Show)
