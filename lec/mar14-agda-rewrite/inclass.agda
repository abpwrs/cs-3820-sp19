module inclass where

open import lib

~~-id : ∀ b → ~ ~ b ≡ b
~~-id tt = refl
~~-id ff = refl

assoc-&& : ∀ x y z → (x && y) && z ≡ x && (y && z)
assoc-&& tt y z = refl
assoc-&& ff y z = refl

ite-lift-~ : ∀ x y z → (if x then ~ y else ~ z) ≡ (~ (if x then y else z))
ite-lift-~ tt y z = refl
ite-lift-~ ff y z = refl

ite-lift : ∀{A : Set}{B : Set}(f : A → B) →
           ∀ x y z →
           (if x then (f y) else (f z)) ≡ (f (if x then y else z))
ite-lift f tt y z = refl
ite-lift f ff y z = refl

postulate
  A : Set
  a b : A
  f : A → A
  p : f a ≡ a
  q : a ≡ b

thm1 : f (f a) ≡ a
thm1 rewrite p = p

thm1alt : f (f a) ≡ a
thm1alt rewrite p | p = refl

thm2 : f (f (f (f (f a)))) ≡ a
thm2 rewrite p | p | p | p | p = refl

thm3 : f (f b) ≡ b
thm3 rewrite sym q | thm1 = refl

thm3alt : f (f b) ≡ b
thm3alt rewrite sym q = thm1 

tt-&& : ∀ x y → (x && y ≡ tt) → (x ≡ tt)
tt-&& tt y p = refl
tt-&& ff y p = p

tt-&&' : ∀ x y → (x && y ≡ tt) → (x ≡ tt)
tt-&&' tt y p = refl
tt-&&' ff y ()  {- absurd pattern, tells Agda to note that this case
                   could never happen so no equation needed -}
