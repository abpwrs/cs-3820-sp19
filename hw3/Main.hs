import System.Environment
import Basics
import Control.Monad.State.Lazy


main :: IO ()
main = do
  l <- prob1
  putStrLn $ show l
  putStrLn $ show $ runState prob2 3
  putStrLn $ show $ prob3 3  
  rargs <- reverseArgs
  putStrLn $ show rargs
  first <- getFirstArgIf
  putStrLn $ show first
