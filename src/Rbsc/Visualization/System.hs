{-# LANGUAGE OverloadedStrings #-}


-- | Visualization of system instances.
module Rbsc.Visualization.System
    ( visualizeSystem
    ) where


import Control.Lens

import Data.Map.Strict           (assocs)
import Data.Maybe                (isJust)
import Data.Text.Prettyprint.Doc

import Rbsc.Data.Name
import Rbsc.Data.System


-- | Create a graph representing a system instance in the Graphviz DOT
-- format.
visualizeSystem :: System -> Doc ann
visualizeSystem sys =
    "digraph system" <+> lbrace <> line <> indent 4 body <> rbrace
  where
    body =
        sep (fmap (visualizeInstance sys) (view (instances.to assocs) sys)) <>
        sep (fmap (visualizeBoundTo sys) (view (boundTo.to assocs) sys))


visualizeInstance :: System -> (ComponentName, TypeName) -> Doc ann
visualizeInstance sys (name, tyName) = case containedRoles name sys of
    []  | isJust (view (containedIn.at name) sys) -> emptyDoc
        | otherwise ->
            ident name <+>
            brackets ("shape=box,label=" <> dquotes instanceLabel) <> semi
    roleNames ->
        ident name <+>
        brackets ("shape=plaintext,label=<" <> table roleNames <> ">") <> semi
  where
    instanceLabel = pretty name <+> colon <+> pretty tyName
    table roleNames =
        "<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">" <>
        "<TR><TD COLSPAN=" <> dquotes (pretty (length roleNames)) <> ">" <>
        instanceLabel <> "</TD></TR>" <>
        "<TR>" <> hsep (fmap visualizeRole roleNames) <> "</TR>" <>
        "</TABLE>"
    visualizeRole roleName =
        "<TD PORT=" <> dquotes (pretty roleName) <> ">" <> roleLabel roleName <>
        "</TD>"
    roleLabel roleName =
        pretty roleName <+>
        colon <+> maybe emptyDoc pretty (view (instances . at roleName) sys)


visualizeBoundTo :: System -> (RoleName, ComponentName) -> Doc ann
visualizeBoundTo sys (roleName, playerName) =
    port roleName <+> "->" <+> port playerName <> semi
  where
    port name =
        case view (containedIn.at name) sys of
            Just compartmentName ->
                ident compartmentName <> colon <> ident name
            Nothing -> ident name


ident :: ComponentName -> Doc ann
ident = dquotes . pretty
