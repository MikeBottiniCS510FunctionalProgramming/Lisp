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
import Control.Monad.State

import Expr

data Scope = Scope {
  globals :: Map.Map String Expr,
  locals :: Map.Map String Expr
} deriving(Show)

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

bIf :: Expr -> Expr -> Expr -> State Scope Expr
bIf cnd x y = do 
  m <- get
  case runState (eval cnd) m of
    (List [], m') -> eval y
    (_, m') -> eval x

-- We don't have macros, so we have to define `cond`, which works on a list of Exprs.
bCond :: [Expr] -> State Scope Expr
bCond [] = state (\m -> (nil, m))
bCond ((List [x, y]):zs) = do
  m <- get
  case runState (eval x) m of
    (List [], m') -> bCond zs
    (_, m') -> eval y
bCond _ = error "current clause must be a two-element list!"


bLambda :: Expr -> Expr -> State Scope Expr
bLambda (List params) body
  | all isKeyword params = do
    m <- get
    return (Closure (locals m) (map keyword params) body)
  | otherwise = error "lambda: malformed expression!"
      where isKeyword (Atom (Keyword _)) = True
            isKeyword _ = False
            keyword (Atom (Keyword s)) = s

bLabel :: String -> Expr -> Expr -> State Scope Expr
bLabel label params body = do
  m <- get
  case runState (bLambda params body) m of
    (f@(Closure captures params body), m') -> do
      put m'
      return (Closure (Map.insert label f captures) params body)

bSet :: Expr -> Expr -> State Scope Expr
bSet (Atom (Keyword s)) expr = do
  m <- get
  let (evaledExpr, m') = runState (eval expr) m
  put m { globals = Map.insert s evaledExpr (globals m') }
  return evaledExpr
bSet _ _ = error "set! expects a keyword!"

bListP :: Expr -> Expr
bListP (List _) = t
bListP _ = nil 

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
  ("cons",  Builtin "cons" $ createBuiltinOnList2 "cons" bCons),
  ("listp", Builtin "listp" $ createBuiltinOnList1 "listp" bListP)]

eval :: Expr -> State Scope Expr

-- If the expression is a number, it evaluates to itself.
eval x@(Atom (Number _)) = return x

-- If the expression is an atom but is a keyword, a lookup occurs.
eval (Atom (Keyword k)) = do
  m <- get
  case Map.lookup k (Map.union (locals m) (globals m)) of
    Just e -> return e
    Nothing -> error ("Lookup error " ++ k)

-- If the expression is a list of expressions, we now perform pattern matching.
eval xs = case xs of
    List [Atom (Keyword "quote"), x] -> return (bQuote x)
    List (Atom (Keyword "quote"):_) -> error "quote: incorrect arity"

    List [Atom (Keyword "if"), cnd, x, y] -> bIf cnd x y
    List (Atom (Keyword "if"):_) -> error "if: incorrect arity"

    List (Atom (Keyword "cond"):xs) -> bCond xs

    List [Atom (Keyword "lambda"), args, body] -> bLambda args body
    List (Atom (Keyword "lambda"):_) -> error "lambda: incorrect arity"

    List [Atom (Keyword "label"), Atom (Keyword label), args, body] -> bLabel label args body
    List (Atom (Keyword "label"):_) -> error "label: incorrect arity"

    List [Atom (Keyword "set!"), Atom (Keyword k), v] -> bSet (Atom (Keyword k)) v
    List [Atom (Keyword "set!"), _, v] -> error "set!: not a keyword!"
    List (Atom (Keyword "set!"):_) -> error "set!: incorrect arity"

    List [Atom (Keyword "eval"), xs] -> do
      m <- get
      let (xs', m') = runState (eval xs) m
      put m'
      return xs'
    List (Atom (Keyword "eval"):_) -> error "eval: incorrect arity"

    List ((Closure captures params body):args) -> do
      m <- get
      let currentLocals = locals m
      let argS = mapM eval args
      let (evaledArgs, m') = runState argS m
      if (length evaledArgs /= length params) 
        then error "closure: incorrect arity"
        else do 
          let (result, m'') = runState (eval body) (m' { locals = Map.unions [Map.fromList (zip params evaledArgs), captures, currentLocals] })
          modify (\m -> m { globals = (globals m'') })
          return result

    List ((Builtin _ f):xs) -> do
      m <- get
      let argS = mapM eval xs
      let (evaledArgs, m') = runState argS m
      put m'
      return (f evaledArgs)
    
    c@(Closure _ _ _) -> return c
    b@(Builtin _ _) -> return b

    List [] -> return nil
    List (Atom (Keyword x):xs) -> do
      m <- get
      let (result, m') = runState (eval (List (((Map.union (locals m) (globals m)) ! x) : xs))) m
      put m'
      return result
    List (x@(Atom _):xs) -> error (show x ++ ": not a function!")
    List (x:xs) -> do
      m <- get
      let (y, m') = runState (eval x) m
      let (result, m'') = runState (eval (List (y : xs))) m'
      put m''
      return result
