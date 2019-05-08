module inclass where

open import lib hiding ( _&&_ ; &&-idem)

_&&_ : 𝔹 → 𝔹 → 𝔹
tt && b' = b'
ff && b' = ff

&&-idem : ∀ b → b && b ≡ b
&&-idem tt = refl
&&-idem ff = refl

-- termination checker will complain for this one:
f : 𝔹 → 𝔹
f tt = tt
f ff = f ff

easy : ∃ ℕ (λ n → n > 0 ≡ tt)
easy = 1 , refl
