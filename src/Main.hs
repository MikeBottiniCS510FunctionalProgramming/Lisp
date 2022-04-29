module Main where

import Expr
import Parser
import Builtins

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map

parseAndEval :: String -> Either ParseError Expr
parseAndEval s = eval Map.empty <$> parse expr "" s

main :: IO ()
main = putStrLn "Hello, Haskell!"
