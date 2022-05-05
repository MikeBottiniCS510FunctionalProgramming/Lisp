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
-- We also define `nil`, `t`, and `cond` for convenience.

module Builtins where

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))

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

bIf :: Map.Map String Expr -> Expr -> Expr -> Expr -> Expr
bIf m cnd x y = case (eval m cnd) of
  (List []) -> eval m y
  _ -> eval m x

-- We don't have macros, so we have to define `cond`, which works on a list of Exprs.
bCond :: Map.Map String Expr -> [Expr] -> Expr
bCond _ [] = nil
bCond m ((List [x, y]):zs) = case (eval m x) of
  (List []) -> bCond m zs
  _ -> eval m y
bCond _ _ = error "current clause must be a two-element list!"


bLambda :: Map.Map String Expr -> Expr -> Expr -> Expr
bLambda captures (List params) body
  | all isKeyword params = Closure captures (map get params) body
  | otherwise = error "lambda: malformed expression!"
      where isKeyword (Atom (Keyword _)) = True
            isKeyword _ = False
            get (Atom (Keyword s)) = s

bLabel :: String -> Map.Map String Expr -> Expr -> Expr -> Expr
bLabel label captures params body = case bLambda captures params body of
  f@(Closure captures params body) -> Closure (Map.insert label f captures) params body

eval :: Map.Map String Expr -> Expr -> Expr

-- If the expression is a number, it evaluates to itself.
eval _ x@(Atom (Number _)) = x

-- If the expression is an atom but is a keyword, a lookup occurs.
eval m (Atom (Keyword k)) = case Map.lookup k m of
    Just e -> e
    Nothing -> error ("Lookup error " ++ k)

-- If the expression is a list of expressions, we now perform pattern matching.
eval m xs = case xs of
    List [Atom (Keyword "quote"), x] -> bQuote x
    List (Atom (Keyword "quote"):_) -> error "quote: incorrect arity"

    List [Atom (Keyword "atom"), x] -> bAtom (eval m x)
    List (Atom (Keyword "atom"):_) -> error "atom: incorrect arity"

    List [Atom (Keyword "eq"), x, y] -> bEq (eval m x) (eval m y)
    List (Atom (Keyword "eq"):_) -> error "eq: incorrect arity"

    List [Atom (Keyword "car"), xs] -> bCar (eval m xs)
    List (Atom (Keyword "car"):_) -> error "car: incorrect arity"

    List [Atom (Keyword "cdr"), xs] -> bCdr (eval m xs)
    List (Atom (Keyword "cdr"):_) -> error "cdr: incorrect arity"

    List [Atom (Keyword "cons"), x, xs] -> bCons (eval m x) (eval m xs)
    List (Atom (Keyword "cons"):_) -> error "cons: incorrect arity"

    List [Atom (Keyword "if"), cnd, x, y] -> bIf m cnd x y
    List (Atom (Keyword "if"):_) -> error "if: incorrect arity"

    List (Atom (Keyword "cond"):xs) -> bCond m xs

    List [Atom (Keyword "lambda"), args, body] -> bLambda m args body
    List (Atom (Keyword "lambda"):_) -> error "lambda: incorrect arity"

    List [Atom (Keyword "label"), Atom (Keyword label), args, body] -> bLabel label m args body
    List (Atom (Keyword "label"):_) -> error "label: incorrect arity"

    List ((Closure captures params body):args) ->
      eval (Map.unions [Map.fromList (zip params args), captures, m]) body
    
    c@(Closure _ _ _) -> c

    List [] -> nil
    List (Atom (Keyword x):xs) -> 
      eval m (List ((m ! x) : map (eval m) xs))
    List (x@(Atom _):xs) -> error (show x ++ ": not a function!")
    List (x:xs) -> eval m (List (eval m x : xs))

