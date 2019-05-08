module Inclass where

data Expr = Num Integer | Plus Expr Expr | Times Expr Expr {-deriving Show -}

-- 1 + (2 + 4)
example = Plus (Num 1) (Plus (Num 2) (Num 4))

-- 2 + (3 * 4)
example2 = Plus (Num 2) (Times (Num 3) (Num 4))

eval :: Expr -> Integer
eval (Num x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Times x y) = (eval x) * (eval y)

data Direction = L | R deriving Show

-- direction deepest in tree is at the head of the path
type Path = [Direction]

showPath :: Path -> String
showPath = concat . map show . reverse

nodeName :: Path -> String
nodeName p = "n" ++ showPath p

labelString :: String -> String
labelString s = " [ label = \"" ++ s ++ "\"]";

edgeString :: Path -> Path -> String
edgeString p1 p2 = nodeName p1 ++ " -> " ++ nodeName p2 ++ ";\n"

declareNode :: Path -> String -> String
declareNode p l = nodeName p ++ labelString l ++ ";\n"

binaryToGraphViz :: Path -> String -> Expr -> Expr -> String
binaryToGraphViz p l x y = declareNode p l ++
                           edgeString p (L : p) ++
                           edgeString p (R : p) ++
                           exprToGraphvizH (L : p) x ++
                           exprToGraphvizH (R : p) y

exprToGraphvizH :: Path -> Expr -> String
exprToGraphvizH p (Num x) = declareNode p (show x)
exprToGraphvizH p (Plus x y) = binaryToGraphViz p "+" x y
exprToGraphvizH p (Times x y) = binaryToGraphViz p "*" x y

exprToGraphviz :: Expr -> String
exprToGraphviz e =
  "digraph G {\n" ++
  "node [shape=plaintext];\n" ++
  exprToGraphvizH [] e ++
  "}\n"

instance Show Expr where
  show = exprToGraphviz 

