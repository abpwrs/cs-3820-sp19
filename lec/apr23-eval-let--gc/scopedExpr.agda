module scopedExpr where

open import lib
open import rawExpr

{- ScopedExpr s is the type for expressions that are allowed
   to use variables in stringset s -}
data ScopedExpr : stringset → Set where
  Var : ∀ (v : string){s : stringset} → stringset-contains s v ≡ tt → ScopedExpr s
  Num : ∀ {s : stringset} → ℕ → ScopedExpr s
  Bool : ∀ {s : stringset} → 𝔹 → ScopedExpr s
  Add : ∀ {s : stringset} → ScopedExpr s → ScopedExpr s → ScopedExpr s
  Lt : ∀ {s : stringset} → ScopedExpr s → ScopedExpr s → ScopedExpr s
  Ite : ∀ {s : stringset} → ScopedExpr s → ScopedExpr s → ScopedExpr s → ScopedExpr s
  Let : ∀ {s : stringset}
          (v : string) {- variable name -} →
          ScopedExpr s {- definition of the variable -} →
          ScopedExpr (stringset-insert s v) {- body of the let -} →
          ScopedExpr s

testScopedExpr : ScopedExpr empty-stringset 
testScopedExpr = Let "x" (Add (Num 2) (Num 2)) (Add (Var "x" refl) (Var "x" refl))

{- does not type check due to scoping violation with "y": 
testScopedExpr2 : ScopedExpr empty-stringset 
testScopedExpr2 = Let "x" (Add (Num 2) (Num 2)) (Add (Var "x" refl) (Var "y" refl))
-}

scopeCheck : ∀(s : stringset) → RawExpr → maybe (ScopedExpr s)
scopeCheck s (Var v) with keep (stringset-contains s v)
scopeCheck s (Var v) | tt , p = just (Var v p)
scopeCheck s (Var v) | ff , _ = nothing
scopeCheck s (Num x) = just (Num x)
scopeCheck s (Bool x) = just (Bool x)
scopeCheck s (Add r r') = scopeCheck s r ≫=maybe (λ e →
                          scopeCheck s r' ≫=maybe (λ e' → just (Add e e')))
scopeCheck s (Lt r r') = scopeCheck s r ≫=maybe (λ e →
                         scopeCheck s r' ≫=maybe (λ e' → just (Lt e e')))
scopeCheck s (Ite r r' r'') =
                         scopeCheck s r ≫=maybe (λ e →
                         scopeCheck s r' ≫=maybe (λ e' → 
                         scopeCheck s r'' ≫=maybe (λ e'' → just (Ite e e' e''))))
scopeCheck s (Let v r r') =
     scopeCheck s r ≫=maybe (λ e →
     scopeCheck (stringset-insert s v) r' ≫=maybe (λ e' → 
       just (Let v e e')))

testScopeChecka = scopeCheck empty-stringset testRawExpr2
testScopeCheckb = scopeCheck empty-stringset testRawExpr3
testScopeCheckc = scopeCheck (stringset-insert empty-stringset "y") testRawExpr3
