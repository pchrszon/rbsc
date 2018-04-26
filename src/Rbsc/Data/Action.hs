-- | Internal representation of actions.
module Rbsc.Data.Action
    ( Action(..)
    ) where


import Rbsc.Data.Name


-- | An action used for synchronization.
data Action
    = Action !Name
    | LocalAction !Name Action
    | IndexedAction Action !Int
    deriving (Show)
