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

prelude :: [String]
prelude = [
  "(set! nil ())",
  "(set! t 't)",
  "(set! foldl (label foldl (f acc xs) (if (atom xs) acc (foldl f (f acc (car xs)) (cdr xs)))))",
  "(set! flip (lambda (f) (lambda (x y) (f y x))))",
  "(set! Y (lambda (h) ((lambda (x) (h (lambda (a) ((x x) a)))) (lambda (x) (h (lambda (a) ((x x) a)))))))",
  "(set! reverse (lambda (xs) (foldl (flip cons) () xs)))",
  "(set! map (lambda (f xs) (reverse (foldl (lambda (acc x) (cons (f x) acc)) () xs))))",
  "(set! concat (lambda (xs ys) (foldl (flip cons) ys (reverse xs))))",
  "(set! filter (lambda (pred xs) (reverse (foldl (lambda (acc x) (if (pred x) (cons x acc) acc)) nil xs))))"]

parseAndEvalS :: String -> StateT Scope (Either ParseError) (Either String Expr)
parseAndEvalS s = StateT (\m ->
    case parseExpr s of
        Left e -> Left e
        Right e -> Right (runState (eval e) m))

parseAndEval :: Scope -> String -> Either ParseError (Either String Expr)
parseAndEval m s = evalStateT (parseAndEvalS s) m

preludeScope :: Scope
preludeScope 
  = fromRight (error "prelude error!") . 
    execStateT (mapM parseAndEvalS prelude) $ 
    Scope (Map.union builtinPrelude arithmeticPrelude) Map.empty

mainLoop :: Scope -> IO ()
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
main = mainLoop preludeScope
