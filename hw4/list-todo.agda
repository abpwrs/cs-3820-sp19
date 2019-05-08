module list-todo where 

open import lib

{-
The pattern of all of these operations re-occurs every 2 elements
Therefore, we only need to 'rewrite' our expression if two elements remina
I just think of this as two base cases
Thinking of it as two base cases makes the proof section ~slightly~ more intutive
-}

-- Functions to write proofs about

-- replaces everyother character with a space
space-every-other : 𝕃 char → 𝕃 char
space-every-other [] = []
space-every-other (a :: []) = a :: []
space-every-other (a :: b :: xs) = a :: ' ' :: (space-every-other xs)

-- swaps every two characters
swap-pairs : 𝕃 char → 𝕃 char
swap-pairs [] = []
swap-pairs (a :: []) = a :: []
swap-pairs (a :: b :: xs) = b :: a :: (swap-pairs xs)

{- 
space-every-other is a length preserving operation
-}
seo-length : ∀ l → length (space-every-other l) ≡ length l
seo-length [] = refl
seo-length (x :: []) = refl
seo-length (x :: x₁ :: l) rewrite seo-length l = refl

{-
the keep idiom -- (page 90)
space-every-other is idempotent
meaning that repeated applications have no effect
-}
seo-idem : ∀ l → space-every-other (space-every-other l) ≡ space-every-other l
seo-idem [] = refl
seo-idem (x :: []) = refl
seo-idem (x :: x₁ :: l) rewrite seo-idem l = refl

{-
Showing that swap-pairs applied twice is just the origional list
same idea as the seo-idem and seo-length for the actual proof
-}
swap-pairs-inv : ∀ l → swap-pairs (swap-pairs l) ≡ l
swap-pairs-inv [] = refl
swap-pairs-inv (x :: []) = refl
swap-pairs-inv (x :: x₁ :: l) rewrite swap-pairs-inv l = refl

{- 
for all lists l, given that l is not of odd length, 
the operation order (seo, swap-pairs, seo) 
will result in an list of spaces of the same length as l
-}
nuke-all : ∀ l → is-odd (length l) ≡ ff → space-every-other (swap-pairs (space-every-other l)) ≡ repeat (length l) ' ' 
nuke-all [] x = refl
nuke-all (x₁ :: []) () -- not possible based on the pre condition is-odd (length l) ≡ ff
nuke-all (x₁ :: x₂ :: l) x rewrite nuke-all l x = refl

-- this should just be an empty list of length 4
test = nuke-all ('a' :: 'b' :: 'c' :: 'd' :: []) refl
