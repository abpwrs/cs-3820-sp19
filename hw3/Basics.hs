module Basics where

import System.Environment
import Control.Monad.State.Lazy


prob1 :: IO [Integer]
prob1 = putStrLn "prob1" >>= return (return [1,2,3])

-- prob1 :: IO [Integer]
-- prob1 =  
--   do
--     putStrLn "prob1"
--     return [1,2,3]

-- prob2 :: State Integer Integer
-- prob2 = get >>= (\ x -> put (x + x) >> return 1)
prob2 :: State Integer Integer
prob2 = do { x <- get; y <- put (x + x); return 1 }

type Sta s a = s -> (s,a)

prob3 :: Sta Integer Integer
prob3 s = (1, (s+s))

reverseArgs :: IO [String]
reverseArgs = do { args <- getArgs; return (reverse args)}

getFirstArgIf :: IO (Maybe String)
getFirstArgIf = do {
    args <- getArgs;
    if length args == 0 then
        return Nothing
        else
            return (Just (head args))
}
