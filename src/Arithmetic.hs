module Arithmetic where

import Expr

bOp :: String -> (Int -> Int -> Int) -> Expr -> Expr -> Expr
bOp msg op x y = case (x, y) of
    (Atom (Number x'), Atom (Number y')) -> Atom (Number (x' `op` y'))
    _ -> error (msg ++ ": values must be integers!")

bAdd :: Expr -> Expr -> Expr
bAdd = bOp "add" (+)

bSub :: Expr -> Expr -> Expr
bSub = bOp "sub" (-)

bMul :: Expr -> Expr -> Expr
bMul = bOp "mul" (*)

bDiv :: Expr -> Expr -> Expr
bDiv = bOp "div" div
