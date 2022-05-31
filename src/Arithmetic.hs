module Arithmetic where

import Expr
import Builtins

import qualified Data.Map.Strict as Map
import Control.Monad

bOp :: String -> (Int -> Int -> Int) -> Expr -> Expr -> Either String Expr
bOp msg op x y = case (x, y) of
    (Atom (Number x'), Atom (Number y')) -> Right $ Atom (Number (x' `op` y'))
    _ -> Left $ msg ++ ": values must be integers!"

bAdd :: Expr -> Expr -> Either String Expr
bAdd = bOp "+" (+)

bSub :: Expr -> Expr -> Either String Expr
bSub = bOp "-" (-)

bMul :: Expr -> Expr -> Either String Expr
bMul = bOp "*" (*)

bDiv :: Expr -> Expr -> Either String Expr
bDiv = bOp "/" div

bMod :: Expr -> Expr -> Either String Expr
bMod = bOp "mod" mod

foldM1Either :: String -> (a -> a -> Either String a) -> [a] -> Either String a
foldM1Either op _ [] = Left $ op ++ ": empty list!"
foldM1Either _ f (x:xs) = foldM f x xs

arithmeticPrelude :: Map.Map String Expr
arithmeticPrelude = Map.fromList [
  ("+", Builtin "+" $ foldM bAdd (Atom (Number 0))),
  ("-", Builtin "-" $ foldM1Either "-" bSub),
  ("*", Builtin "*" $ foldM bMul (Atom (Number 1))),
  ("/", Builtin "/" $ foldM1Either "/" bDiv),
  ("mod", Builtin "mod" $ createBuiltinOnList2 "mod" bMod)]

