module Positional where

type Pos = [Integer]

toPos :: Integer -> Integer -> Pos
toPos _ 0 = []
toPos b n = n `mod` b : toPos b (n `div` b) 

fromPos :: Integer -> Pos -> Integer
fromPos b = foldr (\x y -> x + b*y) 0

data Expr = Num Integer | Plus Expr Expr | Times Expr Expr

instance Show Expr where
  show (Num n) = show n
  show (Plus e f) = "(" ++ show e ++ " + " ++ show f ++ ")"
  show (Times e f) = "(" ++ show e ++ " * " ++ show f ++ ")"

toExpr :: Integer -> Pos -> Expr
toExpr _ [] = Num 0
toExpr _ (x:[]) = Num x
toExpr b l = toExprHelper [Times (Num element) (Num base) | (element, base) <- zip l weights] 
  where weights = iterate (*b) 1

toExprHelper :: [Expr] -> Expr
toExprHelper [e] = e 
toExprHelper [] = Num 0 
toExprHelper (x : xs) = Plus x (toExprHelper xs)

addPos :: Integer -> Pos -> Pos -> Pos
addPos b x y 
  | length x > length y = addHelp b 0 x y
  | otherwise = addHelp b 0 y x

addHelp :: Integer -> Integer -> Pos -> Pos -> Pos
addHelp 0 _ _ _ = []
addHelp _ c [] _ 
  | c /= 0 = [c]
  | otherwise = []
addHelp b c (x:xs) [] 
  | (x+c) >= b = (x+c) `mod` b : addHelp b 1 xs []
  | otherwise = x+c : addHelp b 0 xs []
addHelp b c (x:xs) (y:ys) 
  | (x+y+c) >= b = (x+y+c) `mod` b : addHelp b 1 xs ys
  | otherwise = x+y+c : addHelp b 0 xs ys
