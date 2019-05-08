module Main where

--import Control.Monad.Reader

dummy :: () -> b -> b
dummy _ b = b

appg :: Applicative f => f () -> f b -> f b
x `appg` y = pure dummy <*> x <*> y

anotherMain :: IO ()
anotherMain =
  putStrLn "Hi there" `appg`
  putStrLn "bye" 

{-
getString :: IO String -> String -- cannot get out of the IO monad
getString x =
  do
    s <- x
    x -- not allowed
-}

main :: IO ()
main =
  do
    putStrLn "Hi there"
    x <- getLine
    putStrLn ("You typed: " ++ x)


{-main :: IO ()
main =
  putStrLn "Hi there" -- IO ()
>>
  putStrLn "bye" -- IO ()
-}


{-
main :: IO ()
main = putStrLn "Hello, world!"
-}
