import Grammar
import Expr
import Tokens
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  putStrLn $ show $ parseExpr $ scanTokens s
