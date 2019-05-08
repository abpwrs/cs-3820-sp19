module Graphviz where

import Data.List
import Expr

type Direction = Int

{- the type for Paths in a tree where each node has a finite number of subtrees

   A Path [ dk , ... , d1 ] represents the path in the tree which starts at the
   root and then goes to its d1'th subtree, then the d2'th subtree of that tree,
   and so on.
-}   
newtype Path = Path { getDirections :: [Direction]}

showPath :: Path -> String
showPath = intercalate "a" . map show . reverse . getDirections

instance Show Path where
  show = showPath

nodeName :: Path -> String
nodeName p = "n" ++ show p

labelString :: String -> String
labelString s = " [ label = \"" ++ s ++ "\"]";

edgeString :: Path -> Path -> String
edgeString p1 p2 = nodeName p1 ++ " -> " ++ nodeName p2 ++ ";\n"

declareNode :: Path -> String -> String
declareNode p l = nodeName p ++ labelString l ++ ";\n"

toGraph :: (Show s) => MyTree s -> String
toGraph s = "digraph G {\n" ++ "node [shape-plaintext];\n" ++ toGraphHelper (Path []) 1 s ++ "}\n"

toGraphHelper :: (Show a) => Path -> Int -> MyTree a -> String
toGraphHelper p i (Value a) = declareNode p (show a)
toGraphHelper p i (Head s list) = declareNode p (show s) ++ connections (Path (getDirections p ++ [i])) (i+1) list

connections :: (Show a) => Path -> Int -> [MyTree a] -> String
connections p i l = edgeString p (Path (getDirections p ++ [i])) ++ toGraphHelper p i (head l) ++ connections p (i+1) (tail l)
connections p i [] = ""

