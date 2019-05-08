module Inclass where

import Data.HashMap.Strict
import Data.Char

example :: HashMap Int String
example = insert 4 "hi"
          (insert 5 "bye"
          (insert 6 "what"
          empty))

addLists :: [Int] -> [Int] -> [Int]
addLists [] _ = []
addLists _ [] = []
addLists (x:xs) (y:ys) = (x + y) : addLists xs ys

concatLists :: [String] -> [String] -> [String]
concatLists [] _ = []
concatLists _ [] = []
concatLists (x:xs) (y:ys) = (x ++ y) : concatLists xs ys

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 g [] _ = []
map2 g _ [] = []
map2 g (x:xs) (y:ys) = (g x y) : map2 g xs ys

addLists' = map2 (+)
concatLists' = map2 (++)

map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 = undefined -- how to generalize to any arity? (map4, map5, ...)

map2Maybes :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2Maybes g Nothing _ = Nothing
map2Maybes g _ Nothing = Nothing
map2Maybes g (Just x) (Just y) = Just (g x y)

data Expr = Num Float | Add Expr Expr | Mult Expr Expr | Bomb | Div Expr Expr

-- divide by zero will cause Nothing to be returned
eval :: Expr -> Maybe Float
eval (Num x) = Just x
eval (Add x y) = pure (+) <*> eval x <*> eval y

--yucky alternative:
eval (Mult x y) = case eval x of
                     Nothing -> Nothing
                     Just n -> case eval y of
                                 Nothing -> Nothing
                                 Just m -> Just (n * m)
eval Bomb = Nothing

eval (Div x y) = case eval y of
                   Nothing -> Nothing
                   Just 0 -> Nothing
                   Just n -> pure (/) <*> eval x <*> Just n
