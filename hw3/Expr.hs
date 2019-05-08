module Expr where

-- this is the same as Data.Tree
data MyTree a = Value a  | Head a [MyTree a]
    deriving Show
