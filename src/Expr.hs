-- An Expr is either an Atom or a List of Expressions.
-- For now, an Atom is either a keyword or an integer.
-- After that, we'll hopefully add closures and a numerical tower.

module Expr where

import Data.List(intercalate)

data Atom = Number Int | Keyword String deriving Eq

instance Show Atom where
  show (Number x) = show x
  show (Keyword s) = s

data Expr = Atom Atom | List [Expr]

instance Show Expr where
  show (Atom a) = show a
  show (List xs) = ('(':) . (++")") . intercalate " " . map show $ xs

