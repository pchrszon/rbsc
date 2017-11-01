{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskellQuotes #-}


module Rbsc.Parser.TH
    ( model
    , expr
    ) where


import Control.Exception
import Control.Monad.Identity

import Text.Megaparsec (eof)

import           Data.Data
import           Data.Generics
import           Data.Text     (Text)
import qualified Data.Text     as Text

import Language.Haskell.TH       hiding (Loc)
import Language.Haskell.TH.Quote

import           Rbsc.Parser
import           Rbsc.Parser.Lexer          (run, sc)
import qualified Rbsc.Parser.Expr as Parser

import           Rbsc.Report
import qualified Rbsc.Report.Error.Syntax as Error
import           Rbsc.Report.Region

import Rbsc.Syntax.ComponentType
import Rbsc.Syntax.Constant
import Rbsc.Syntax.Expr.Untyped
import Rbsc.Syntax.Model
import Rbsc.Syntax.Operators

import Rbsc.Name


-- | Quasi quoter that parses the given string into a 'Model'.
model :: QuasiQuoter
model = QuasiQuoter
    { quoteExp = \str -> do
        m <- runIO (parseIO str)
        dataToExpQ (const Nothing `extQ` handleText) m
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }


expr :: QuasiQuoter
expr = QuasiQuoter
    { quoteExp = \str -> do
        let (result, _) = runIdentity (run exprParser "splice" (Text.pack str))
        case result of
            Left err -> fail (show err)
            Right e -> dataToExpQ (const Nothing `extQ` handleText) e
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
  where
    exprParser = sc *> Parser.expr <* eof


-- | Convert 'Text' to a string literal and apply 'Text.pack'.
--
-- see <https://stackoverflow.com/q/12788181>
handleText :: Text -> Maybe ExpQ
handleText = Just . appE (varE 'Text.pack) . litE . StringL . Text.unpack


parseIO :: String -> IO Model
parseIO str = do
    result <- parse "splice" (Text.pack str)
    case result of
        Left errors -> throwIO (userError (unlines (printErrors errors)))
        Right m     -> return m


printErrors :: [Error.Error] -> [String]
printErrors = fmap (show . render . Error.toReport)


deriving instance Data Model

deriving instance Data ConstantDef
deriving instance Data ConstantType

deriving instance Data NaturalTypeDef
deriving instance Data RoleTypeDef
deriving instance Data CompartmentTypeDef

deriving instance Data Expr
deriving instance Data ArithOp
deriving instance Data EqOp
deriving instance Data RelOp
deriving instance Data LogicOp
deriving instance Data Quantifier

deriving instance Data Position
deriving instance Data Region
deriving instance Data a => Data (Loc a)

deriving instance Data TypeName
