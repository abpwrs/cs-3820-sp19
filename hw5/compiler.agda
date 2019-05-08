module compiler where

open import expr
open import lib

eqTp≡ : ∀ {T1 T2} → eqTp T1 T2 ≡ tt → T1 ≡ T2
eqTp≡ {TpBool} {TpBool} x = refl
eqTp≡ {TpNat} {TpBool} ()
eqTp≡ {TpBool} {TpNat} ()
eqTp≡ {TpNat} {TpNat} x = refl

tpCheck : RawExpr → maybe (Σ Tp (λ T → Expr T))
tpCheck (Num x) = just(TpNat , Num x)

tpCheck (Bool x) = just(TpBool , Bool x)

{- here is one example case in the style I found easiest to deal with (I suggest
   implementing the cases for Lt and Ite similarly). -}
tpCheck (Add r r') with tpCheck r | tpCheck r' 
tpCheck (Add r r') | just(TpNat , e1) | just(TpNat , e2) = just(TpNat , Add e1 e2)
tpCheck (Add r r') | _ | _ = nothing

tpCheck (Lt r r') with tpCheck r | tpCheck r'
tpCheck (Lt r r') | just(TpNat , e1) | just(TpNat , e2) = just(TpBool , Lt e1 e2)
tpCheck (Lt r r') | _ | _ = nothing

tpCheck (Ite i t e) with tpCheck i | tpCheck t | tpCheck e
tpCheck (Ite i t e) | just(TpBool , e1) | just(TpNat , e2) | just(TpNat , e3) = just(TpNat , Ite e1 e2 e3)
tpCheck (Ite i t e) | just(TpBool , e1) | just(TpBool , e2) | just(TpBool , e3) = just(TpBool , Ite e1 e2 e3)
tpCheck (Ite i t e) | _ | _ | _ = nothing


confirm : tpCheck testRawExpr ≡ just (TpBool , testExpr)
confirm = refl

data AddExpr : Set where
  Num : ℕ → AddExpr
  AddNum : ℕ → AddExpr → AddExpr

data LtExpr : Tp → Set where
  injAddExpr : AddExpr → LtExpr TpNat
  Lt : AddExpr → AddExpr → LtExpr TpBool
  Bool : 𝔹 → LtExpr TpBool

data IteExpr : Tp → Set where
  injLtExpr : ∀{T : Tp} → LtExpr T → IteExpr T
  Ite : ∀{T : Tp} → LtExpr TpBool → IteExpr T → IteExpr T → IteExpr T


fixAddition : AddExpr → AddExpr → AddExpr
fixAddition (Num x) x₁ = AddNum x x₁
fixAddition (AddNum x x₂) x₁ = AddNum x (fixAddition x₂ x₁)

addIte : IteExpr TpNat → IteExpr TpNat → IteExpr TpNat
addIte (injLtExpr (injAddExpr x)) (injLtExpr (injAddExpr x₁)) = injLtExpr (injAddExpr (fixAddition x x₁))
addIte (injLtExpr x) (Ite x₁ x₂ x₃) = Ite x₁ (addIte x₂ (injLtExpr x)) (addIte x₃ (injLtExpr x))
addIte (Ite x x₁ x₂) (injLtExpr x₃) = Ite x (addIte (injLtExpr x₃) x₁) (addIte (injLtExpr x₃) x₂)
addIte (Ite x x₁ x₂) (Ite x₃ x₄ x₅) = Ite x (Ite x₃ (addIte (x₁) (x₄)) (addIte (x₁) (x₅))) (Ite x₃ (addIte (x₂) (x₄)) (addIte (x₂) (x₅))) 



ltIte : IteExpr TpNat → IteExpr TpNat -> IteExpr TpBool
ltIte (injLtExpr (injAddExpr x)) (injLtExpr (injAddExpr x₁)) = injLtExpr (Lt x x₁)
ltIte (injLtExpr x) (Ite x₁ x₂ x₃) = Ite x₁ (ltIte x₂ (injLtExpr x)) (ltIte x₃ (injLtExpr x))
ltIte (Ite x x₁ x₂) (injLtExpr x₃) = Ite x (ltIte (injLtExpr x₃) x₁) (ltIte (injLtExpr x₃) x₂)
ltIte (Ite x x₁ x₂) (Ite x₃ x₄ x₅) = Ite x (Ite x₃ (ltIte (x₁) (x₄)) (ltIte (x₁) (x₅))) (Ite x₃ (ltIte (x₂) (x₄)) (ltIte (x₂) (x₅)))


toIteExpr : ∀{T : Tp} → Expr T → IteExpr T
toIteExpr (Num x) =  injLtExpr (injAddExpr (Num x)) 
toIteExpr (Bool x) = injLtExpr (Bool x)
toIteExpr (Add x x₁) = addIte (toIteExpr x) (toIteExpr x₁)
toIteExpr (Lt x x₁) = ltIte (toIteExpr x) (toIteExpr x₁)
toIteExpr (Ite x x₁ x₂) = Ite (Bool (eval x)) (toIteExpr x₁) (toIteExpr x₂)





