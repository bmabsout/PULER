{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Parser(module Parser) where

import Data.Void
import Control.Applicative
import Control.Monad
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as M
import Data.Char
import Data.Bifunctor as Bifunctor
import Data.Function
import Data.Either
import qualified Data.List.NonEmpty as N
import Control.Monad.Combinators.Expr
import Data.Foldable

import Core
import Parsable
import Prettyprinter (Pretty(pretty))

keywords = ["if", "then", "else", "in", "fix"]

isKeyword n = n `elem` keywords

instance Parsable Var where
  parser = do
    var <- M.label "a variable" ((N.:|) <$> M.letterChar <*> M.takeWhileP Nothing isAlphaNum)
    guard (not $ isKeyword (N.toList var))
    -- annotation <- 
    return (Var var) M.<?> "a variable"

instance (Parsable var, Parsable expr) => Parsable (Dec var expr) where
  parser = Dec <$> parser <*> (midOp "=" *> parser)

instance Parsable a => Parsable (Decs a) where
  parser = Decs <$> parser `sepBy1` midOp ";"

instance Parsable Lit where
  parser =
      (Lint <$> L.decimal)
      <|> (Lbool <$> boolParser)
      <|> (Lstring <$> stringParser)
      <|> (Lunit <$ M.string "{}")
    where
      stringParser :: Parser String
      stringParser = M.string "\"" <> M.takeWhileP Nothing (\x -> x /= '\'' && x /= '\"') <> M.string "\""
      boolParser :: Parser Bool
      boolParser =
            (True <$ keyword "True")
        <|> (False <$ keyword "False")

instance Parsable a => Parsable (Let a) where
  parser = Let
      <$> (begKeyword "let" *> parser)
      <*> (midKeyword "in" *> parser)


instance Parsable a => Parsable (If a) where
  parser = If
      <$> (begKeyword "if"   *> parser)
      <*> (midKeyword "then" *> parser)
      <*> (midKeyword "else" *> parser)

instance Parsable (N.NonEmpty Var) where
  parser = parser `sepBy1` spaceForcer

instance (Parsable args, Parsable body) => Parsable (Lambda args body) where
  parser = Lambda
      <$> (begOp "\\" *> parser)
      <*> (midOp "->" *> parser)

instance (Parsable body) => Parsable (Fixer body) where
  parser = Fixer <$> (begKeyword "fix" *> parser <* spaceConsumer) <*> parser


typesParser :: Parsable name => Parser (Types name)
typesParser =
    M.try ((:->) <$> (parens typesParser <|> Tbase <$> parser)
                 <* midOp "->"
                 <*> typesParser)
    <|> (Tbase <$> parser)

instance Parsable StdType where
  parser = allInhabitants &> (\x -> (pretty x & show & M.string & M.try) &> const x) & asum

instance (Parsable a, Parsable b) => Parsable (PossibleAnno a b) where
  parser = M.try (Annoed <$> parser) <|> NotAnnoed <$> parser

instance Parsable name => Parsable (Types name) where
  parser = typesParser

instance (Parsable expr, Parsable var) => Parsable (Annotated expr var) where
  parser = annoParser parser

annoParser :: Parsable var => Parser expr -> Parser (Annotated expr var)
annoParser partParser = Annotated <$> partParser <* midOp ":" <*> parser

ops = [ [ prefix "-" Neg
        , prefix "+" Pos
        ]
      , [ binary "*" Mul
        ]
      , [ binary "+" Add
        , binary "-" Sub
        ]
      , [ binary "==" EqInt
        ]
      ]

binary :: String -> StdFunction -> Operator Parser Expr
binary name f = InfixL (M.try $ binaryExpr <$ midOp name)
  where binaryExpr x y = app (app (var $ show f) x) y

prefix :: String -> StdFunction -> Operator Parser Expr
prefix name f = Prefix (M.try $ app (var $ show f) <$ midOp name)

exprParser :: Parser Expr
exprParser = makeExprParser (M.try (Eannotation <$> annoParser appAndParensParser) <|> appAndParensParser) ops

appAndParensParser :: Parser Expr
appAndParensParser =
  parens (foldl1 (\x y -> Eapp (App x y)) <$> (exprParser `sepBy1` spaceForcer))
  <|> noRecParser


instance Parsable Expr where
  parser = exprParser

noRecParser :: Parser Expr
noRecParser =
    Efix         <$> parser
    <|> Elambda  <$> parser
    <|> Elet     <$> parser
    <|> Eif      <$> parser
    <|> Elit     <$> parser
    <|> Evar     <$> parser

parseHelper :: Parsable parseMe => String -> Either String parseMe
parseHelper s = first M.errorBundlePretty $ M.parse (parser <* M.eof) "" s

parseDecs :: String -> Either String (Decs Expr)
parseDecs = parseHelper

unsafeParse :: String -> Expr
unsafeParse s = case parseHelper s of
    Right x -> x
    Left e -> error e