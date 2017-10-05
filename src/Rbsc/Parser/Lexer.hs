{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}


-- | Parsers for tokens.
module Rbsc.Parser.Lexer
    ( reservedWords
    , reserved
    , identifier
    , symbol
    , lexeme
    , sc
    , loc
    ) where


import Control.Applicative
import Control.Monad       (void)

import Data.String

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Prim

import qualified Rbsc.SourceSpan as S


-- | List of reserved keywords.
reservedWords :: [String]
reservedWords =
    [
    ]


-- | Parser for a reserved word.
reserved :: (MonadParsec e s m, Token s ~ Char) => String -> m ()
reserved s = lexeme (symbol s *> notFollowedBy alphaNumChar)


-- | Parser for an identifier.
identifier :: (MonadParsec e s m, Token s ~ Char, IsString a) => m a
identifier = lexeme . try $ do
    ident <- (:) <$> letterChar <*> many alphaNumChar
    if ident `elem` reservedWords
        then fail $ "unexpected reserved word " ++ ident
        else return (fromString ident)


-- | Parser for a symbol.
symbol :: (MonadParsec e s m, Token s ~ Char) => String -> m String
symbol = L.symbol sc


-- | Parse the given lexeme and skip any following white space.
lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme = L.lexeme sc


-- | Parser for non-empty white space (including newlines).
sc :: (MonadParsec e s m, Token s ~ Char) => m ()
sc = L.space (void spaceChar) (L.skipLineComment "//") empty


-- | Annotate a parsed value with its 'SourceSpan' in the source.
loc :: (MonadParsec e s m) => m (S.SourceSpan -> a) -> m a
loc p = do
    from <- convert <$> getPosition
    x <- p
    to <- convert <$> getPosition
    return $ x (S.SourceSpan from to)
  where
    convert (SourcePos _ line col) = S.SourcePos (fromPos line) (fromPos col)
    fromPos = fromIntegral . unPos
