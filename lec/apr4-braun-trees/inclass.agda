module inclass where

open import lib
open import braun-tree ℕ _<_

test : braun-tree 2
test = bt-node 1 (bt-node 2 bt-empty bt-empty (inj₁ refl)) bt-empty (inj₂ refl)


-- cannot fill this hole because this is not a legal Braun tree
test2 : braun-tree 2
test2 = bt-node 1 bt-empty (bt-node 2 bt-empty bt-empty (inj₁ refl)) {!!}

data Tp : Set where
  TpBool : Tp
  TpNat : Tp

data Expr : Tp → Set where
  Num : ℕ → Expr TpNat
  Bool : 𝔹 → Expr TpBool
  Add : Expr TpNat → Expr TpNat → Expr TpNat
  Lt : Expr TpNat → Expr TpNat → Expr TpBool
  Ite : ∀{T : Tp} → Expr TpBool → Expr T → Expr T → Expr T


testExpr : Expr TpBool
testExpr = Ite (Bool tt) (Lt (Num 3) (Num 4)) (Bool tt)

interpTp : Tp → Set
interpTp TpBool = 𝔹
interpTp TpNat = ℕ

eval : ∀{T : Tp} → Expr T → interpTp T
eval (Num x) = x
eval (Bool x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Lt x y) = (eval x) < (eval y)
eval (Ite i t e) = if (eval i) then eval t else eval e
