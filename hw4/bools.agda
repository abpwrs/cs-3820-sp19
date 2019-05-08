module bools where

open import lib

-- both expression evaluate down, you just have to case on the booleans
triple-~ : ∀ b → ~ ~ ~ b ≡ ~ b
triple-~ tt = refl
triple-~ ff = refl

-- expression evaluates down too bools, you just have to case on the booleans
idem-|| : ∀ b → b || b ≡ b
idem-|| tt = refl
idem-|| ff = refl

-- this is overkill, but it was easiest to just case on all of the booleans
&&-distrib : ∀ x y z → x && (y || z) ≡ (x && y) || (x && z)
&&-distrib tt tt tt = refl
&&-distrib tt tt ff = refl
&&-distrib tt ff tt = refl
&&-distrib tt ff ff = refl
&&-distrib ff tt tt = refl
&&-distrib ff tt ff = refl
&&-distrib ff ff tt = refl
&&-distrib ff ff ff = refl

-- this is overkill, but it was easiest to just case on all of the booleans
||-distrib : ∀ x y z → x || (y && z) ≡ (x || y) && (x || z)
||-distrib tt tt tt = refl
||-distrib tt tt ff = refl
||-distrib tt ff tt = refl
||-distrib tt ff ff = refl
||-distrib ff tt tt = refl
||-distrib ff tt ff = refl
||-distrib ff ff tt = refl
||-distrib ff ff ff = refl

{-
again, just case on the booleans, the rest of the set stuff is fine, 
and really just a result of the boolean
-}
ite-not : ∀(A : Set)(x : 𝔹)(y : A)(z : A) → if x then y else z ≡ if ~ x then z else y
ite-not A tt a b = refl
ite-not A ff b a = refl



