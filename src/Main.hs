module Main where

import Expr
import Parser
import Builtins
import Arithmetic

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as Map
import Data.Either
import Control.Monad.State
import System.IO
import qualified Control.Exception as Exc

prelude1 = Map.union builtinPrelude $
           fromRight (error "eval error!") . 
           parseAndEval builtinPrelude <$> Map.fromList [
    ("nil", "()"),
    ("t", "'t"),
    ("foldl", "(label foldl (f acc xs) (if (atom xs) acc (foldl f (f acc (car xs)) (cdr xs))))"),
    ("flip", "(lambda (f) (lambda (x y) (f y x)))"),
    ("Y", "(lambda (h) ((lambda (x) (h (lambda (a) ((x x) a)))) (lambda (x) (h (lambda (a) ((x x) a))))))")]

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

prelude4 = Map.union prelude3 arithmeticPrelude

parseAndEvalS :: String -> StateT (Map.Map String Expr) (Either ParseError) Expr
parseAndEvalS s = StateT (\m ->
    case parseExpr s of
        Left e -> Left e
        Right e -> Right (runState (eval e) m))

parseAndEval :: Map.Map String Expr -> String -> Either ParseError Expr
parseAndEval m s = evalStateT (parseAndEvalS s) m

parseAndEvalPrelude :: String -> Either ParseError Expr
parseAndEvalPrelude s = evalStateT (parseAndEvalS s) prelude4

mainLoop :: Map.Map String Expr -> IO ()
mainLoop m = do
  putStr "> "
  hFlush stdout
  line <- Exc.try getLine :: IO (Either Exc.SomeException String)
  case line of
    Left _ -> putStrLn "Ceterum censeo Carthaginem esse delendam."
    Right ":state" -> do
      putStrLn . show $ m
      mainLoop m
    Right line -> do
      case runStateT (parseAndEvalS line) m of
        (Left e) -> do
          putStrLn . show $ e
          mainLoop m
        Right (expr, m') -> do
          putStrLn . show $ expr
          mainLoop m'

main :: IO ()
main = mainLoop prelude4
