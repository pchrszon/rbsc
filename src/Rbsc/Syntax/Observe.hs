-- | Abstract syntax for observable definitions.
module Rbsc.Syntax.Observe
    ( Observe(..)
    ) where


import Data.List.NonEmpty (NonEmpty)


-- | A definition of observable roles.
newtype Observe expr = Observe
    { obsRoles :: NonEmpty expr
    } deriving (Show)
