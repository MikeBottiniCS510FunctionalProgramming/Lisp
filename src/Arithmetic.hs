module Arithmetic where

import Expr
import Builtins

import qualified Data.Map.Strict as Map

bOp :: String -> (Int -> Int -> Int) -> Expr -> Expr -> Expr
bOp msg op x y = case (x, y) of
    (Atom (Number x'), Atom (Number y')) -> Atom (Number (x' `op` y'))
    _ -> error (msg ++ ": values must be integers!")

bAdd :: Expr -> Expr -> Expr
bAdd = bOp "+" (+)

bSub :: Expr -> Expr -> Expr
bSub = bOp "-" (-)

bMul :: Expr -> Expr -> Expr
bMul = bOp "*" (*)

bDiv :: Expr -> Expr -> Expr
bDiv = bOp "/" div

bMod :: Expr -> Expr -> Expr
bMod = bOp "mod" mod

arithmeticPrelude :: Map.Map String Expr
arithmeticPrelude = Map.fromList [
  ("+", Builtin "+" $ foldl bAdd (Atom (Number 0))),
  ("-", Builtin "-" $ foldl1 bSub),
  ("*", Builtin "*" $ foldl bMul (Atom (Number 1))),
  ("/", Builtin "/" $ foldl1 bDiv),
  ("mod", Builtin "mod" $ createBuiltinOnList2 "mod" bMod)]

