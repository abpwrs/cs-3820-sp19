module value where

open import lib
open import tp

data Value : Tp → Set where
  Num : ℕ → Value TpNat
  Bool : 𝔹 → Value TpBool

interpValue : ∀{T : Tp} → Value T → interpTp T
interpValue (Num x) = x
interpValue (Bool x) = x
