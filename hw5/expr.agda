module expr where

open import lib 

data Tp : Set where
  TpBool : Tp
  TpNat : Tp

eqTp : Tp → Tp → 𝔹
eqTp TpBool TpBool = tt
eqTp TpBool TpNat = ff
eqTp TpNat TpBool = ff
eqTp TpNat TpNat = tt

data Expr : Tp → Set where
  Num : ℕ → Expr TpNat
  Bool : 𝔹 → Expr TpBool
  Add : Expr TpNat → Expr TpNat → Expr TpNat
  Lt : Expr TpNat → Expr TpNat → Expr TpBool
  Ite : ∀{T : Tp} → Expr TpBool → Expr T → Expr T → Expr T

testExpr : Expr TpBool
testExpr = Ite (Bool tt) (Lt (Num 3) (Num 4)) (Bool tt)

testExpr2 : Expr TpNat
testExpr2 = Add (Ite (Bool tt) (Add (Num 3) (Num 4)) (Num 5)) (Num 6)

testExpr3 : Expr TpNat
testExpr3 = Add (Ite (Bool tt) (Num 3) (Num 4)) (Ite (Bool ff) (Num 5) (Num 6))

testExpr4 : Expr TpNat
testExpr4 = Add (Ite (Lt (Ite (Bool tt) (Num 3) (Num 4)) (Num 5)) (Num 6) (Num 7)) (Ite (Bool ff) (Num 8) (Num 9))

interpTp : Tp → Set
interpTp TpBool = 𝔹
interpTp TpNat = ℕ

eval : ∀{T : Tp} → Expr T → interpTp T
eval (Num x) = x
eval (Bool x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Lt x y) = (eval x) < (eval y)
eval (Ite i t e) = if (eval i) then eval t else eval e

data RawExpr : Set where
  Num : ℕ → RawExpr
  Bool : 𝔹 → RawExpr
  Add : RawExpr → RawExpr → RawExpr
  Lt : RawExpr → RawExpr → RawExpr
  Ite : RawExpr → RawExpr → RawExpr → RawExpr

testRawExpr : RawExpr
testRawExpr = Ite (Bool tt) (Lt (Num 3) (Num 4)) (Bool tt)

erase : ∀ {T} → Expr T → RawExpr
erase (Num x) = Num x
erase (Bool x) = Bool x
erase (Add e e') = Add (erase e) (erase e')
erase (Lt e e') = Lt (erase e) (erase e')
erase (Ite e e' e'') = Ite (erase e) (erase e') (erase e'')
