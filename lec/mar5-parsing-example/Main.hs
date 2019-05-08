import Grammar
import Expr
import Tokens
import System.Environment
import Typing

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  let typing = parseTyping $ scanTokens s 
  putStrLn (show typing) 
  putStrLn (if check typing then "Checks" else "Does not check")
