-- An Expr is either an Atom or a List of Expressions.
-- For now, an Atom is either a keyword or an integer.
-- After that, we'll hopefully add closures and a numerical tower.

module Expr where

import Data.List(intercalate)
import qualified Data.Map.Strict as Map

data Atom = Number Int | Keyword String deriving Eq

instance Show Atom where
  show (Number x) = show x
  show (Keyword s) = s

data Expr = Atom Atom |
            List [Expr] | 
            Closure (Map.Map String Expr) [String] Expr |
            Builtin String ([Expr] -> Either String Expr)

instance Show Expr where
  show (Atom a) = show a
  show (List []) = "nil"
  show (List xs) = ('(':) . (++")") . intercalate " " . map show $ xs
  show (Closure captures args expr)
    = intercalate " " ["Closure", show args, show expr]
  show (Builtin label _) = "Builtin " ++ label

