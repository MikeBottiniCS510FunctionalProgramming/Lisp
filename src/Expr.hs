-- An Expr is either an Atom or a List of Expressions.
-- For now, an Atom is either a keyword or an integer.
-- After that, we'll hopefully add closures and a numerical tower.

module Expr where

data Atom = Number Int | Keyword String deriving(Show, Eq)

data Expr = Atom Atom | List [Expr] deriving(Show)

