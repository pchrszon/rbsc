{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


-- | Errors occuring during evaluation of expressions.
module Rbsc.Report.Error.Eval
    ( Error(..)
    , toReport

    , _DivisionByZero
    , _NotConstant
    ) where


import Control.Lens

import Rbsc.Report
import Rbsc.Report.Region

-- | Represents an evaluation error.
data Error
    = DivisionByZero !Region
    | NotConstant !Region
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


makePrisms ''Error