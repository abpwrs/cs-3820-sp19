module expr where

open import lib 

open import rawExpr
open import context
open import econtext
open import tp
open import value

data Expr : context → Tp → Set where
  Var : ∀(v : string){G : context}{T : Tp} → context-lookup G v ≡ just T → Expr G T
  Num : ∀ {G : context} → ℕ → Expr G TpNat
  Bool : ∀ {G : context} → 𝔹 → Expr G TpBool
  Add : ∀ {G : context} → Expr G TpNat → Expr G TpNat → Expr G TpNat
  Lt : ∀ {G : context} → Expr G TpNat → Expr G TpNat → Expr G TpBool
  Ite : ∀{G : context}{T : Tp} → Expr G TpBool → Expr G T → Expr G T → Expr G T
  Let : ∀{G : context}
         {T T' : Tp}
         (v : string) {- variable name -} → 
         Expr G T {- definition of variable -} →
         Expr (context-insert G v T) T' →
         Expr G T'

testExpr : Expr empty-context TpBool
testExpr = Ite (Bool tt) (Lt (Num 3) (Num 4)) (Bool tt)

testExpr2 : Expr empty-context TpNat
testExpr2 = Let "x" (Num 3) (Add (Var "x" refl) (Var "x" refl))

{- fails to typecheck in Agda due to object-language type error 
testExpr3 : Expr empty-context TpNat
testExpr3 = Let "x" (Num 3) (Ite (Var "x" refl) (Num 3) (Num 4))-}

typeCheck : ∀(G : context) → RawExpr → maybe (Σ Tp (λ T → Expr G T))
typeCheck G (Var v) with keep (context-lookup G v)
typeCheck G (Var v) | just T , p = just (T , Var v p)
typeCheck G (Var v) | nothing , p = nothing
typeCheck G (Num n) = just (TpNat , Num n)
typeCheck G (Bool b) = just (TpBool , Bool b)
typeCheck G (Add r r') with typeCheck G r | typeCheck G r'
typeCheck G (Add r r') | just (TpNat , e) | just (TpNat , e') = just (TpNat , Add e e')
typeCheck G (Add r r') | _ | _ = nothing
typeCheck G (Lt r r') with typeCheck G r | typeCheck G r'
typeCheck G (Lt r r') | just (TpNat , e) | just (TpNat , e') = just (TpBool , Lt e e')
typeCheck G (Lt r r') | _ | _ = nothing
typeCheck G (Ite r r' r'') with typeCheck G r | typeCheck G r' | typeCheck G r''
typeCheck G (Ite r r' r'') | just (TpBool , e) | just (T' , e') | just (T'' , e'') = {!!}
typeCheck G (Ite r r' r'') | _ | _ | _ = nothing
typeCheck G (Let v r r') with typeCheck G r 
typeCheck G (Let v r r') | just (T , e) with typeCheck (context-insert G v T) r'
typeCheck G (Let v r r') | just (T , e) | just (T' , e') = just (T' , Let v e e')
typeCheck G (Let v r r') | just (T , e) | nothing = nothing
typeCheck G (Let v r r') | nothing = nothing



inj-just : ∀{A : Set}{a b : A} → just a ≡ just b → a ≡ b
inj-just refl = refl


eval : ∀(E : econtext){T : Tp} → Expr (to-context E) T → interpTp T
eval E (Num x) = x
eval E (Bool x) = x
eval E (Add x y) = (eval E x) + (eval E y)
eval E (Lt x y) = (eval E x) < (eval E y)
eval E (Ite i t e) = if (eval E i) then eval E t else eval E e
eval E (Var v p) with keep (econtext-lookup E v)
eval E (Var v p) | just (T , v') , q rewrite lookup-to-context E (string-to-𝕃char v) q | inj-just p = interpValue v'
eval E {T} (Var v p) | nothing , q rewrite lookup-to-context E (string-to-𝕃char v) q with p
eval E {T} (Var v p) | nothing , q | ()
eval E _ = {!!}
 
