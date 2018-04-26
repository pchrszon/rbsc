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

import Language.Haskell.TH       hiding (Loc, Body)
import Language.Haskell.TH.Quote


import Rbsc.Data.ComponentType
import Rbsc.Data.Function (FunctionName (..))
import Rbsc.Data.Name

import           Rbsc.Parser
import           Rbsc.Parser.Lexer          (run, sc)
import qualified Rbsc.Parser.Expr as Parser

import           Rbsc.Report
import           Rbsc.Report.Error          (Error(..))
import qualified Rbsc.Report.Error as Error
import           Rbsc.Report.Region
import           Rbsc.Report.Result (toEither)

import           Rbsc.Syntax.Untyped hiding (Type(..))
import qualified Rbsc.Syntax.Type as Syntax


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
    case toEither result of
        Left errors -> throwIO (userError (unlines (printErrors errors)))
        Right m     -> return m


printErrors :: [Error] -> [String]
printErrors = fmap (show . render . Error.toReport)


deriving instance Data Model

deriving instance Data expr => Data (Constant expr)

deriving instance Data expr => Data (Function expr)
deriving instance Data expr => Data (Parameter expr)

deriving instance Data expr => Data (Global expr)

deriving instance Data expr => Data (VarDecl expr)

deriving instance Data NaturalTypeDef
deriving instance Data RoleTypeDef
deriving instance Data expr => Data (CompartmentTypeDef expr)
deriving instance Data expr => Data (MultiRole expr)

deriving instance Data expr => Data (Syntax.Type expr)
deriving instance Data expr => Data (VarType expr)

deriving instance (Data expr, Data comp) => Data (Implementation comp expr)
deriving instance (Data expr, Data comp) => Data (ImplBody comp expr)
deriving instance (Data expr, Data comp) => Data (Module comp expr)
deriving instance (Data expr, Data comp) => Data (ModuleBody comp expr)
deriving instance (Data expr, Data comp) => Data (Command comp expr)
deriving instance (Data expr, Data comp) => Data (Update comp expr)
deriving instance Data expr => Data (Assignment expr)

deriving instance (Typeable a, Data expr, Data comp, Data (a expr)) => Data (Body a comp expr)
deriving instance (Typeable a, Data expr, Data comp, Data (a expr)) => Data (BodyItem a comp expr)
deriving instance (Typeable a, Data expr, Data comp, Data (a expr)) => Data (Loop a comp expr)

deriving instance Data Expr
deriving instance Data FunctionName
deriving instance Data ArithOp
deriving instance Data EqOp
deriving instance Data RelOp
deriving instance Data LogicOp
deriving instance Data Quantifier
deriving instance (Data comp, Data expr) => Data (QuantifiedType comp expr)

deriving instance Data ComponentTypeSet

deriving instance Data Position
deriving instance Data Region
deriving instance Data a => Data (Loc a)

deriving instance Data TypeName
