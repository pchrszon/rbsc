{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Errors occuring during evaluation of expressions.
module Rbsc.Report.Error.Eval
    ( Error(..)
    , toReport

    , _DivisionByZero
    , _NotConstant
    , _IndexOutOfBounds
    , _ExceededDepth
    ) where


import Control.Lens

import Data.Semigroup
import qualified Data.Text as Text


import Rbsc.Report
import Rbsc.Report.Region

-- | Represents an evaluation error.
data Error
    = DivisionByZero !Region
    | NotConstant !Region
    | IndexOutOfBounds !Int !Int !Region
    | ExceededDepth !Region
    deriving (Eq, Show)


toReport :: Error -> Report
toReport = \case
    DivisionByZero rgn ->
        Report "division by zero"
            [ errorPart rgn $ Just
                "division by zero occurred while evaluating this expression"
            ]

    NotConstant rgn ->
        Report "expression is not constant" [ errorPart rgn Nothing ]

    IndexOutOfBounds len idx rgn ->
        Report "index out of bounds"
            [ errorPart rgn . Just $
                "array has size " <> Text.pack (show len) <>
                "but the index is " <> Text.pack (show idx)
            ]

    ExceededDepth rgn ->
        Report "exceeded maximum recursion depth"
            [ errorPart rgn . Just $
                "exceeded recursion depth evaluating this expression"
            ]


makePrisms ''Error
