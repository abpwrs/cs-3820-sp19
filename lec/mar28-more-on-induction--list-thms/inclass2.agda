module inclass2 where

open import lib
 
concat-map-singleton : ∀ {A : Set}(l : 𝕃 A) → concat (map [_] l) ≡ l 
concat-map-singleton {A} [] = refl
concat-map-singleton {A} (x :: l) rewrite concat-map-singleton l = refl

length-filter' : ∀{A : Set}(l : 𝕃 A)(p : A → 𝔹) → length (filter p l) ≤ length l ≡ tt
length-filter' [] p = refl
length-filter' (x :: l) p with p x
length-filter' (x :: l) p | tt = length-filter' l p
length-filter' (x :: l) p | ff = ≤-suc-trans{length (filter p l)} (length-filter' l p)

filter-idempotent : ∀{A : Set}(l : 𝕃 A)(p : A → 𝔹) → filter p (filter p l) ≡ filter p l
filter-idempotent [] p = refl
filter-idempotent (x :: l) p with keep (p x)
filter-idempotent (x :: l) p | tt , q rewrite q | q | filter-idempotent l p = refl
filter-idempotent (x :: l) p | ff , q rewrite q = filter-idempotent l p
