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

createBuiltinOnList1 :: String -> (Expr -> Expr) -> [Expr] -> Expr
createBuiltinOnList1 _ f [x] = f x
createBuiltinOnList1 msg f _ = error (msg ++ " is an arity-1 function!")

createBuiltinOnList2 :: String -> (Expr -> Expr -> Expr) -> [Expr] -> Expr
createBuiltinOnList2 _ f [x, y] = f x y
createBuiltinOnList2 msg f _ = error (msg ++ " is an arity-2 function!")

builtinPrelude :: Map.Map String Expr
builtinPrelude = Map.fromList [
  ("atom",  Builtin "atom" $ createBuiltinOnList1 "atom" bAtom),
  ("eq",    Builtin "eq" $ createBuiltinOnList2 "eq" bEq),
  ("car",   Builtin "car" $ createBuiltinOnList1 "car" bCar),
  ("cdr",   Builtin "cdr" $ createBuiltinOnList1 "cdr" bCdr),
  ("cons",  Builtin "cons" $ createBuiltinOnList2 "cons" bCons)]

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

    List [Atom (Keyword "if"), cnd, x, y] -> bIf m cnd x y
    List (Atom (Keyword "if"):_) -> error "if: incorrect arity"

    List (Atom (Keyword "cond"):xs) -> bCond m xs

    List [Atom (Keyword "lambda"), args, body] -> bLambda m args body
    List (Atom (Keyword "lambda"):_) -> error "lambda: incorrect arity"

    List [Atom (Keyword "label"), Atom (Keyword label), args, body] -> bLabel label m args body
    List (Atom (Keyword "label"):_) -> error "label: incorrect arity"

    List ((Closure captures params body):args) ->
      eval (Map.unions [Map.fromList (zip params (map (eval m) args)), captures, m]) body

    List ((Builtin _ f):xs) -> f . map (eval m) $ xs
    
    c@(Closure _ _ _) -> c
    b@(Builtin _ _) -> b

    List [] -> nil
    List (Atom (Keyword x):xs) -> 
      eval m (List ((m ! x) : xs))
    List (x@(Atom _):xs) -> error (show x ++ ": not a function!")
    List (x:xs) -> eval m (List (eval m x : xs))

