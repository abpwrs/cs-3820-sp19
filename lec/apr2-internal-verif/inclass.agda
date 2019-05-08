module inclass where

open import lib

postulate
  A : Set
  a b c : A
  f : A → A → A
  p : f a a ≡ a
  q : ∀ x → f x b ≡ x
  r : f a a ≡ c

lem : f (f a a) b ≡ a
lem rewrite p | q a = refl

lem' : a ≡ f (f a a) b 
lem' rewrite q (f a a) | p = refl

lem'' : a ≡ f (f a a) (f a a) 
lem'' rewrite p | p = refl

lem''' : a ≡ f (f a a) (f a a) 
lem''' rewrite p = sym p

lem4 : a ≡ c
lem4 rewrite sym p | sym r = refl 

lem5 : a ≡ c
lem5 with r
lem5 | r' rewrite p = r'

lem6 : c ≡ f a a
lem6 = sym r

div2 : ℕ → ℕ
div2 zero = zero
div2 (suc zero) = zero
div2 (suc (suc x)) = suc (div2 x)

2*div2 : ∀ x → is-even x ≡ tt → 2 * div2 x ≡ x
2*div2 zero p = refl
2*div2 (suc zero) ()
2*div2 (suc (suc x)) p rewrite +suc (div2 x) (div2 x + zero) | 2*div2 x p = refl

example-vector : 𝕍 ℕ 4
example-vector = 1 :: 2 :: 3 :: 10 :: []

length-singleton : ∀ {A : Set}(x : A) → length [ x ] ≡ 1
length-singleton x = refl

_++𝕍'_ : ∀ {ℓ} {A : Set ℓ}{n m : ℕ} → 𝕍 A n → 𝕍 A m → 𝕍 A (n + m)
[]        ++𝕍' ys = ys
(x :: xs) ++𝕍' ys = x :: (xs ++𝕍' ys)

intersperse : ∀{A : Set}{n : ℕ} → A → 𝕍 A n → 𝕍 A (2 * n ∸ 1)
intersperse x [] = []
intersperse x (a :: []) = a :: []
intersperse{A}{suc (suc n)} x (a :: a' :: v) = a :: {!x :: (intersperse x (a' :: v))!}
