module Expr where

data Type = One | Product Type Type | Sum Type Type
  deriving (Show , Eq)

data Term = Iden | Comp Type Term Term | Unit
  deriving Show

data Typing = Typing Term Type {- input type -} Type {- output type -}
  deriving Show
