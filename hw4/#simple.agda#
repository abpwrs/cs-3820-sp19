open import lib

{-
I don't know if you read these files, 
but this was actually a really fun problem
and I learned a good deal by working through this
-}

postulate
  A : Set
  a : A
  b : A
  c : A
  d : A    
  f : A → A
  g : A → A → A
  h : A → A → A
  p : a ≡ b
  q : b ≡ c
  r : f a ≡ a
  s : ∀ x y → g y x ≡ h x x
  t : ∀ x → f x ≡ x → g x x ≡ x -- THIS ONE HAS THE CONDITION FOR LEMMA4
  u : h (f d) (f d) ≡ f d

-- reflz for dayz
lemma0 : c ≡ c
lemma0 = refl

{-
sym q 
turns 
c -> b

sym p
turns 
b -> a
-}
lemma1 : c ≡ a
lemma1 rewrite sym q | sym p = refl

{-
sym p
turns
b -> a

r
turns
f a -> a
-}
lemma2 : f b ≡ b
lemma2 rewrite sym p | r = refl

{-
r
turns
f a -> a

r
turns
f a -> a

s a b
turns
g b a -> h a a
-}
lemma3 : g b (f (f a)) ≡ h a a
lemma3 rewrite r | r | s a b  = refl

{-
s (f d) a
turns
g a (f d) -> h (f d) (f d)

u
turns
h (f d) (f d) -> (f d)

t matches on the precodition f (f d) ≡ (f d)
leaving us with (f d) ≡ (f d)
-}
lemma4 : f (f d) ≡ (f d) → g a (f d) ≡ f d
lemma4 t rewrite s (f d) a | u  = refl

