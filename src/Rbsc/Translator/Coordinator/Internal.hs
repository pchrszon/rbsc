{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}


module Rbsc.Translator.Coordinator.Internal
    ( rolesInConstraint
    ) where


import Control.Lens

import           Data.Maybe (mapMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as Set


import Rbsc.Data.Component
import Rbsc.Data.Some

import Rbsc.Report.Region

import Rbsc.Syntax.Typed


rolesInConstraint :: LSomeExpr -> Set RoleName
rolesInConstraint (Loc (SomeExpr e _) _) =
    Set.fromList (mapMaybe getRole (universeExpr e))
  where
    getRole :: Some Expr -> Maybe RoleName
    getRole = \case
        Some (IsPlayed (Literal comp _)) -> Just (view compName comp)
        _ -> Nothing
