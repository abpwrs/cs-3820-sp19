module Expr where

data E = V String | B E String | A E E
  deriving Show
