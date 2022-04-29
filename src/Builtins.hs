-- Built-in functions have to be defined in Haskell-land; we have to start
-- from somewhere when building our Lisp! The seven are:
-- 
-- quote
-- atom
-- eq
-- car
-- cdr
-- cons
-- if
--
-- We also define `nil` and `t` for convenience.

module Builtins where

import Expr

-- Nil is the empty list.
nil :: Expr
nil = List []

-- T is just a plain ol' keyword.
t :: Expr
t = Atom (Keyword "T")

bQuote :: Expr -> Expr
bQuote = id

bAtom :: Expr -> Expr
bAtom (Atom _) = t
bAtom (List []) = t
bAtom _ = nil

bEq :: Expr -> Expr -> Expr
bEq (Atom x) (Atom y) = if x == y then t else nil
bEq (List []) (List []) = t
bEq _ _ = nil

bCar :: Expr -> Expr
bCar (Atom _) = error "car expects a list!"
bCar (List []) = nil
bCar (List (x:_)) = x

bCdr :: Expr -> Expr
bCdr (Atom _) = error "cdr expects a list!"
bCdr (List []) = nil
bCdr (List (_:xs)) = List xs

bCons :: Expr -> Expr -> Expr
bCons x (Atom _) = error "cons expects a list!"
bCons x (List xs) = List (x:xs)

bIf :: Expr -> Expr -> Expr -> Expr
bIf cnd x y = case cnd of
    (List []) -> y
    _ -> x