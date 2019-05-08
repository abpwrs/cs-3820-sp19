module tp where

open import lib

data Tp : Set where
  TpBool : Tp
  TpNat : Tp

eqTp : Tp → Tp → 𝔹
eqTp TpBool TpBool = tt
eqTp TpBool TpNat = ff
eqTp TpNat TpBool = ff
eqTp TpNat TpNat = tt

interpTp : Tp → Set
interpTp TpBool = 𝔹
interpTp TpNat = ℕ

