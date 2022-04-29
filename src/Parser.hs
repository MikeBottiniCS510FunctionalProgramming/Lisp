module Parser where

import Expr
import Text.ParserCombinators.Parsec

number :: Parser Atom
number = (Number . read) <$> many1 digit

keyword :: Parser Atom
keyword = Keyword <$> many1 (alphaNum <|> oneOf "!$%&*+-./:<=>?@^_~")

atom :: Parser Atom
atom = number <|> keyword

list :: Parser Expr
list = List <$> between (char '(') (char ')') (sepBy expr spaces)

quoted :: Parser Expr
quoted = do
  first <- (char '\'')
  rest <- expr
  pure $ List $ [Atom (Keyword "quote"), rest]

expr :: Parser Expr
expr = list <|> (Atom <$> atom) <|> quoted

