{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}


-- | Construction of symbol tables.
module Rbsc.SymbolTable
    ( SymbolTable
    , fromDeclarations
    ) where


import Control.Lens
import Control.Monad.State

import           Data.Foldable   (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Rbsc.ComponentType       (ComponentTypes)
import qualified Rbsc.Report.Error.Syntax as Syntax
import           Rbsc.Report.Region       (Loc (..), Region)
import           Rbsc.Syntax.Constraint
import           Rbsc.Syntax.Declaration
import           Rbsc.Type


-- | The symbol table holds the type of each identifier in the model
-- source.
type SymbolTable = Map Name AType

data BuilderState = BuilderState
    { _symbols :: Map Name (AType, Region)
    , _errors  :: [Syntax.Error]
    }

makeLenses ''BuilderState


-- | Extract a 'SymbolTable' from a list of 'Declaration's.
fromDeclarations ::
       ComponentTypes -> [Declaration] -> Either [Syntax.Error] SymbolTable
fromDeclarations types decls = runBuilder (components types decls)


type Builder a = State BuilderState a

runBuilder :: Builder a -> Either [Syntax.Error] SymbolTable
runBuilder m =
    let BuilderState syms errs = execState m (BuilderState Map.empty [])
    in if null errs
        then Right (Map.map fst syms)
        else Left errs


-- | Add component instances defined within the system block to the symbol
-- table.
components :: ComponentTypes -> [Declaration] -> Builder ()
components types decls = forOf_ (traverse._DeclSystem) decls $ \cs ->
    for_ cs $ \case
        Loc (HasType (Loc (Variable name) rgnVar) (Loc tyName rgnTy)) _ ->
            lookupComponentType types tyName rgnTy >>= \case
                Just aTy -> insert name aTy rgnVar
                Nothing  -> return ()
        _ -> return ()


-- | Look up the component type with the given name. If the type does not
-- exist, an error is thrown and @Nothing@ is returned.
lookupComponentType ::
       ComponentTypes -> TypeName -> Region -> Builder (Maybe AType)
lookupComponentType types tyName rgn
    | Map.member tyName types =
        return (Just (AType (TyComponent tyName Map.empty)))
    | otherwise = do
        throw (Syntax.UndefinedType rgn)
        return Nothing


-- | Insert an identifier with a given 'Name' and 'AType' into the symbol
-- table. Throws an error if the identifier has been defined already.
insert :: Name -> AType -> Region -> Builder ()
insert name aTy rgn =
    use (symbols.at name) >>= \case
        Just (_, rgnFirst) -> throw (Syntax.DuplicateIdentifier rgn rgnFirst)
        Nothing -> symbols.at name .= Just (aTy, rgn)


throw :: Syntax.Error -> Builder ()
throw e = modifying errors (++ [e])
