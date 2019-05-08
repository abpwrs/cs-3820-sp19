import Grammar
import Expr
import Tokens
import System.Environment
import Graphviz

main :: IO ()
main = do
  args <- getArgs
  s <- readFile $ head args
  writeFile ((head args) ++ ".gv") $ toGraph $ parseExpr $ scanTokens s

