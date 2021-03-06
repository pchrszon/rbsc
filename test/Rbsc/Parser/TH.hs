{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
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
import           Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as Text

import Language.Haskell.TH       hiding (Loc, Body)
import Language.Haskell.TH.Quote


import Rbsc.Data.Action
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
        let (result, _) =
                runIdentity (run exprParser "splice" (Text.pack str) Map.empty)
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
    (result, _) <- parse "splice" (Text.pack str) []
    case toEither result of
        Left errors -> throwIO (userError (unlines (printErrors errors)))
        Right m     -> return m


printErrors :: [Error] -> [String]
printErrors = fmap (show . render . Error.toReport)


deriving instance Data Model

deriving instance Data expr => Data (Constant expr)

deriving instance Data Enumeration

deriving instance Data expr => Data (Function expr)
deriving instance Data expr => Data (Parameter expr)

deriving instance Data expr => Data (VarDecl expr)

deriving instance Data NaturalTypeDef
deriving instance Data RoleTypeDef
deriving instance Data expr => Data (CompartmentTypeDef expr)
deriving instance Data expr => Data (MultiRole expr)
deriving instance Data TypeSetDef

deriving instance Data expr => Data (Syntax.Type expr)
deriving instance Data expr => Data (VarType expr)

deriving instance Data expr => Data (Label expr)

deriving instance (Data expr, Data ty, Data vars) => Data (Implementation ElemMulti vars ty expr)
deriving instance (Data expr, Data ty, Data vars) => Data (ImplBody ElemMulti vars ty expr)
deriving instance Data expr => Data (ModuleRef expr)
deriving instance (Data expr, Data ty, Data vars) => Data (Module ElemMulti vars ty expr)
deriving instance (Data expr, Data ty, Data vars) => Data (ModuleBody ElemMulti vars ty expr)
deriving instance (Data expr, Data ty, Data vars, Data arg) => Data (ModuleInstance ElemMulti vars ty expr arg)
deriving instance (Data expr, Data ty) => Data (Command ElemMulti ty expr)
deriving instance (Data expr, Data ty) => Data (Update ElemMulti ty expr)
deriving instance Data expr => Data (Assignment expr)

deriving instance Data ActionKind
deriving instance Data ActionIntent

deriving instance (Data expr, Data ty) => Data (RewardStruct ElemMulti ty expr)
deriving instance Data expr => Data (RewardStructItem expr)
deriving instance Data expr => Data (RewardKind expr)

deriving instance (Data ty, Data expr, Data a) => Data (Elem ty expr a)
deriving instance (Data ty, Data expr, Data a) => Data (ElemMulti ty expr a)
deriving instance (Data ty, Data expr, Data a) => Data (Loop ty expr a)

deriving instance (Data expr, Data ty, Data vars) => Data (Coordinator ElemMulti vars ty expr)
deriving instance (Data expr, Data ty) => Data (CoordCommand ElemMulti ty expr)
deriving instance Data expr => Data (PlayingConstraint expr)

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
