module Rbsc.Parser.Declaration
    ( ErrorOrDecl
    ) where


import Text.Megaparsec

import Rbsc.Report.Region
import Rbsc.Syntax.Declaration


type ErrorOrDecl = Either (ParseError Char Dec) (Declaration Region)
