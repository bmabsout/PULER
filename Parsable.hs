module Parsable where

import Data.Void
import Control.Applicative
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char
import qualified Data.List.NonEmpty as N


type Parser = M.Parsec Void String

class Parsable a where
  parser :: Parser a

instance Parsable a => Parsable (Maybe a) where
  parser = M.try (Just <$> parser) <|> return Nothing

instance (Parsable a, Parsable b) => Parsable (Either a b) where
  parser = M.try (Left <$> parser) <|> (Right <$> parser)

instance (Parsable a, Parsable b) => Parsable (a,b) where
  parser = (,) <$> parser <*> parser

sepBy1 parser seperator = N.fromList <$> M.sepEndBy1 parser (M.try seperator)

spaceConsumer = L.space M.space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
spaceForcer = M.try $ (M.space1 <|> orWeirdChar) *> spaceConsumer
  where orWeirdChar = M.lookAhead (M.satisfy (not . isAlphaNum)) *> return ()

whiteAround x = M.between spaceConsumer spaceConsumer x
whiteAround' x = M.between spaceForcer spaceForcer x

keyword :: String -> Parser String
keyword k = M.string k

parens = M.between (begOp "(") (endOp ")")
braces = M.between (begOp "{") (endOp "}")

begOp k = keyword k *> spaceConsumer
midOp k = spaceConsumer *> (keyword k) <* spaceConsumer
endOp k = spaceConsumer *> keyword k

begKeyword k = keyword k *> spaceForcer
midKeyword k = spaceConsumer *> (keyword k) <* spaceForcer
endKeyword k = spaceForcer *> keyword k