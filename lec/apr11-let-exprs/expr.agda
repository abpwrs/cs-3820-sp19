module expr where

open import lib 

open import rawExpr

data Tp : Set where
  TpBool : Tp
  TpNat : Tp

eqTp : Tp → Tp → 𝔹
eqTp TpBool TpBool = tt
eqTp TpBool TpNat = ff
eqTp TpNat TpBool = ff
eqTp TpNat TpNat = tt

context : Set
context = trie Tp -- map strings to Tps

empty-context : context
empty-context = empty-trie

context-lookup : context → string → maybe Tp
context-lookup = trie-lookup

context-insert : context → string → Tp → context
context-insert = trie-insert

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

interpTp : Tp → Set
interpTp TpBool = 𝔹
interpTp TpNat = ℕ

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


{-
eval : ∀{T : Tp} → Expr T → interpTp T
eval (Num x) = x
eval (Bool x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Lt x y) = (eval x) < (eval y)
eval (Ite i t e) = if (eval i) then eval t else eval e


-}
