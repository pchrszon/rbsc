{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}


-- | Construction of symbol tables.
module Rbsc.Data.SymbolTable
    ( SymbolTable
    , fromModel
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Foldable   (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Rbsc.Data.ComponentType (ComponentTypes)
import Rbsc.Data.Name
import Rbsc.Data.Type

import qualified Rbsc.Report.Error.Syntax as Syntax
import           Rbsc.Report.Region       (Loc (..), Region)

import qualified Rbsc.Syntax.Model   as Model
import           Rbsc.Syntax.Untyped hiding (Model (..), Type (..))


-- | The symbol table holds the type of each identifier in the model
-- source.
type SymbolTable = Map Name SomeType


-- | The @BuilderState@ holds the location-annotated symbol table and the
-- list of errors encountered so far.
data BuilderState = BuilderState
    { _symbols :: Map Name (SomeType, Region)
    , _errors  :: [Syntax.Error]
    }

makeLenses ''BuilderState


-- | Extract a 'SymbolTable' from a 'Model'.
fromModel :: ComponentTypes -> UModel -> Either [Syntax.Error] SymbolTable
fromModel types model = runBuilder (components types (Model.system model))


type Builder a = State BuilderState a

runBuilder :: Builder a -> Either [Syntax.Error] SymbolTable
runBuilder m =
    let BuilderState syms errs = execState m (BuilderState Map.empty [])
    in if null errs
        then Right (Map.map fst syms)
        else Left errs


-- | Add component instances defined within the system block to the symbol
-- table.
components :: ComponentTypes -> [LExpr] -> Builder ()
components types es = for_ es $ \case
    Loc (HasType (Loc (Identifier name) rgnVar) (Loc tyName rgnTy)) _ ->
        lookupComponentType types tyName rgnTy >>= \case
            Just ty -> insert name ty rgnVar
            Nothing -> return ()
    _ -> return ()


-- | Look up the component type with the given name. If the type does not
-- exist, an error is thrown and @Nothing@ is returned.
lookupComponentType ::
       ComponentTypes -> TypeName -> Region -> Builder (Maybe SomeType)
lookupComponentType types tyName rgn
    | Map.member tyName types =
        return (Just (SomeType (TyComponent (Just tyName))))
    | otherwise = do
        throw (Syntax.UndefinedType rgn)
        return Nothing


-- | Insert an identifier with a given 'Name' and 'SomeType' into the symbol
-- table. Throws an error if the identifier has been defined already.
insert :: Name -> SomeType -> Region -> Builder ()
insert name ty rgn =
    use (symbols.at name) >>= \case
        Just (_, rgnFirst) -> throw (Syntax.DuplicateIdentifier rgn rgnFirst)
        Nothing -> symbols.at name .= Just (ty, rgn)


throw :: Syntax.Error -> Builder ()
throw e = modifying errors (++ [e])
