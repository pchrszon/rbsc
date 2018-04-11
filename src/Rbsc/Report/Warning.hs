module Rbsc.Report.Warning where


import Rbsc.Report
import Rbsc.Report.Region


data Warning = Warning
    { _warningRegion :: !Region
    , _warningDesc   :: !WarningDesc
    } deriving (Eq, Show)


data WarningDesc
    = WarningDesc
    deriving (Eq, Show)


toReport :: Warning -> Report
toReport = undefined
