module Inclass where

prob1 :: IO [Integer]
prob1 =
  do
    putStrLn "prob1"
    return [1,2,3]

prob1sol :: IO [Integer]
prob1sol =
  putStrLn "prob1" >>= (\ u -> return [1,2,3])

prob1sol2 :: IO [Integer]
prob1sol2 =
  putStrLn "prob1" >> return [1,2,3]
