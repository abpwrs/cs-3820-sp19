open import lib

init𝕍 : ∀{A : Set}{n : ℕ} → 𝕍 A (suc n) → 𝕍 A n
init𝕍 {A} {n} (x :: []) = []
init𝕍 {A} {suc n} (x :: xs) = x :: init𝕍 xs

last𝕍 : ∀{A : Set}{n : ℕ} → 𝕍 A (suc n) → A
last𝕍 {A} {n} (x :: []) = x
last𝕍 {A} {suc n} (x :: xs) = last𝕍 xs

{- take the first m elements from the given input vector, where you are also
   given a proof that m ≤ n. -}
take𝕍 : ∀{A : Set}{n : ℕ} → (m : ℕ) → m ≤ n ≡ tt → 𝕍 A n → 𝕍 A m
take𝕍 {A} {n} zero x x₁ = []
take𝕍 {A} {zero} (suc m) ()
take𝕍 {A} {n} (suc m) x (x₁ :: x₂) = x₁ :: take𝕍 m x x₂


