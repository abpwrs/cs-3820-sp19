module rawExpr where

open import lib

data RawExpr : Set where
  Var : string → RawExpr
  Num : ℕ → RawExpr
  Bool : 𝔹 → RawExpr
  Add : RawExpr → RawExpr → RawExpr
  Lt : RawExpr → RawExpr → RawExpr
  Ite : RawExpr → RawExpr → RawExpr → RawExpr
  Let : string {- variable name -} →
        RawExpr {- definition of the variable -} →
        RawExpr {- body of the let -} →
        RawExpr

testRawExpr : RawExpr
testRawExpr = Ite (Bool tt) (Lt (Num 3) (Num 4)) (Bool tt)

{- let x = 2 + 2 in x + x -}
testRawExpr2 : RawExpr
testRawExpr2 = Let "x" (Add (Num 2) (Num 2)) (Add (Var "x") (Var "x"))

{- let x = 2 + 2 in x + x -}
testRawExpr3 : RawExpr
testRawExpr3 = Let "x" (Add (Num 2) (Num 2)) (Add (Var "x") (Var "y"))


