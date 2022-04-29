module Parser where

import Expr
import Text.ParserCombinators.Parsec

number :: Parser Atom
number = (Number . read) <$> many1 digit

keyword :: Parser Atom
keyword = do
  first <- letter
  rest <- many alphaNum
  pure $ Keyword $ first : rest

atom :: Parser Atom
atom = number <|> keyword

list :: Parser Expr
list = List <$> between (char '(') (char ')') (sepBy expr spaces)

expr :: Parser Expr
expr = list <|> (Atom <$> atom)

