module Expr where

data Expr = Num Integer | Add Expr Expr | Mult Expr Expr | Var String | Let String Expr Expr

-- showExprOp stringForOp e1 e2
showExprOp :: String -> Expr -> Expr -> String
showExprOp opstring e1 e2 = "(" ++ showExpr e1 ++ opstring ++ showExpr e2 ++ ")"

showExpr :: Expr -> String
showExpr (Var x) = x
showExpr (Num x) = show x
showExpr (Add e1 e2) = showExprOp " + " e1 e2
showExpr (Mult e1 e2) = showExprOp " * " e1 e2
showExpr (Let x e1 e2) = "(let " ++ x ++ " = " ++ showExpr e1 ++ " in " ++ showExpr e2 ++ ")"

instance Show Expr where
  show = showExpr

