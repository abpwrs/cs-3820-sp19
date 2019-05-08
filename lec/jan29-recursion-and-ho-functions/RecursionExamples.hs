module RecursionExamples where
import Data.Ord

myTake :: Int -> [a] -> [a]
myTake n [] = []
myTake n (x : xs) | n <= 0 = []
myTake n (x : xs) | otherwise = x : myTake (n - 1) xs

myTake2 :: Int -> [a] -> [a]
myTake2 n l | null l = []
myTake2 n l | n <= 0 = []
myTake2 n l | otherwise = head l : myTake2 (n-1) (tail l)

myInit :: [a] -> [a]
myInit [x] = []
myInit (x : xs) = x : myInit xs

flipOrdering :: Ordering -> Ordering
flipOrdering LT = GT
flipOrdering EQ = EQ
flipOrdering GT = LT
