module inclass where

open import lib

ite-+1 : ∀ (b : 𝔹)(x y z : ℕ) →
        (if b then x else y) + z ≡ if b then (x + z) else (y + z)
ite-+1 tt x y z = refl
ite-+1 ff x y z = refl

ite-+2 : ∀ (b : 𝔹)(x y z : ℕ) →
         z + (if b then x else y) ≡ if b then (z + x) else (z + y)
ite-+2 b x y z
  rewrite +comm z (if b then x else y) | +comm z x | +comm z y =
  ite-+1 b x y z

+-zero : ∀ (x : ℕ) → x + 0 ≡ x
+-zero zero = refl
+-zero (suc x) rewrite +-zero x = refl

+-suc : ∀ (x y : ℕ) → x + (suc y) ≡ suc (x + y)
+-suc zero y = refl
+-suc (suc x) y rewrite +-suc x y = refl

+-comm : ∀ (x y : ℕ) → x + y ≡ y + x
+-comm zero y = sym (+-zero y)
+-comm (suc x) y rewrite +-suc y x | +-comm x y = refl

data Expr : Set where
  Num : ℕ → Expr
  Bool : 𝔹 → Expr
  Add : Expr → Expr → Expr
  Lt : Expr → Expr → Expr
  Ite : Expr → Expr → Expr → Expr

eval : Expr → maybe (ℕ ⊎ 𝔹)
eval (Num n) = just (inj₁ n)
eval (Bool b) = just (inj₂ b)
eval (Add x y) = eval x ≫=maybe (λ vx →
                 eval y ≫=maybe (λ vy →
                 add-maybe vx vy))
  where add-maybe : ℕ ⊎ 𝔹 → ℕ ⊎ 𝔹 → maybe (ℕ ⊎ 𝔹)
        add-maybe (inj₁ x) (inj₁ y) = just (inj₁ (x + y))
        add-maybe _ _ = nothing
eval (Lt x y) = eval x ≫=maybe (λ vx →
                 eval y ≫=maybe (λ vy →
                 add-maybe vx vy))
  where add-maybe : ℕ ⊎ 𝔹 → ℕ ⊎ 𝔹 → maybe (ℕ ⊎ 𝔹)
        add-maybe (inj₁ x) (inj₁ y) = just (inj₂ (x < y))
        add-maybe _ _ = nothing
eval (Ite x y z) = eval x ≫=maybe h
  where h : ℕ ⊎ 𝔹 → maybe (ℕ ⊎ 𝔹)
        h (inj₁ x) = nothing
        h (inj₂ x) = if x then eval y else eval z

thm : ∀ x y z w →
      eval (Add (Ite x y z) w) ≡ eval (Ite x (Add y w) (Add z w))  {- theorem was wrong in mar26 materials! -}
thm x y z w with keep (eval x)
thm x y z w | just (inj₁ x₁) , p rewrite p = refl
thm x y z w | just (inj₂ tt) , p rewrite p with eval y 
thm x y z w | just (inj₂ tt) , p | nothing = refl
thm x y z w | just (inj₂ tt) , p | just (inj₁ vy) with eval w
thm x y z w | just (inj₂ tt) , p | just (inj₁ vy) | nothing = refl
thm x y z w | just (inj₂ tt) , p | just (inj₁ vy) | just (inj₁ vw) = refl
thm x y z w | just (inj₂ tt) , p | just (inj₁ vy) | just (inj₂ vw) = refl
thm x y z w | just (inj₂ tt) , p | just (inj₂ vy) = refl
thm x y z w | just (inj₂ ff) , p rewrite p with eval z
thm x y z w | just (inj₂ ff) , p | nothing = refl
thm x y z w | just (inj₂ ff) , p | just (inj₁ vz) with eval w
thm x y z w | just (inj₂ ff) , p | just (inj₁ vz) | nothing = refl
thm x y z w | just (inj₂ ff) , p | just (inj₁ vz) | just (inj₁ vw) = refl
thm x y z w | just (inj₂ ff) , p | just (inj₁ vz) | just (inj₂ vw) = refl
thm x y z w | just (inj₂ ff) , p | just (inj₂ vz) = refl
thm x y z w | nothing , p rewrite p = refl

 
