module Main where

import Expr
import Parser
import Builtins

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import Data.Either

parseAndEval :: Map.Map String Expr -> String -> Either ParseError Expr
parseAndEval m s = eval m <$> parse expr "" s

prelude1 = Map.union builtinPrelude $
           fromRight (error "eval error!") . 
           parseAndEval builtinPrelude <$> Map.fromList [
    ("nil", "()"),
    ("t", "'t"),
    ("foldl", "(label foldl (f acc xs) (if (atom xs) acc (foldl f (f acc (car xs)) (cdr xs))))"),
    ("flip", "(lambda (f) (lambda (x y) (f y x)))")]

prelude2 = Map.union prelude1 $
           fromRight (error "eval error!") . 
           parseAndEval prelude1 <$> Map.fromList [
    ("reverse", "(lambda (xs) (foldl (flip cons) () xs))")]
    
prelude3 = Map.union prelude2 $
           fromRight (error "eval error!") . 
           parseAndEval prelude2 <$> Map.fromList [
    ("map", "(lambda (f xs) (reverse (foldl (lambda (acc x) (cons (f x) acc)) () xs)))"),
    ("concat", "(lambda (xs ys) (foldl (flip cons) ys (reverse xs)))"),
    ("filter", "(lambda (pred xs) (reverse (foldl (lambda (acc x) (if (pred x) (cons x acc) acc)) nil xs)))")]


parseAndEvalPrelude :: String -> Either ParseError Expr
parseAndEvalPrelude s = eval prelude3 <$> parse expr "" s

main :: IO ()
main = putStrLn "Hello, Haskell!"
