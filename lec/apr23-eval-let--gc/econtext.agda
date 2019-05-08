{-# OPTIONS --allow-unsolved-metas #-}
module econtext where

open import context
open import lib
open import tp
open import value

econtext : Set
econtext = trie (Σ Tp Value)

empty-econtext : econtext
empty-econtext = empty-trie

econtext-lookup : econtext → string → maybe (Σ Tp Value)
econtext-lookup = trie-lookup

econtext-contains : econtext → string → 𝔹
econtext-contains = trie-contains

econtext-insert : econtext → string → (Σ Tp Value) → econtext
econtext-insert = trie-insert

econtext-fsts : (Σ Tp Value) → Tp
econtext-fsts (t , v) = t

to-context : econtext → context
to-context E = trie-map econtext-fsts E

lookup-to-context : ∀(E : econtext)(v : 𝕃 char) → ∀{p : maybe (Σ Tp Value)} →
                    trie-lookup-h E v ≡ p →
                    trie-lookup-h (to-context E) v ≡ maybe-map econtext-fsts p
lookup-to-context (Node x _) [] q rewrite q = refl
lookup-to-context (Node x ts) (c :: v) q with cal-lookup ts c
lookup-to-context (Node x ts) (c :: v) q | nothing rewrite sym q = {!!}
lookup-to-context (Node x ts) (c :: v) q | just t = {!!}
{-
lookup-to-context (Node x ((a , E') :: x₃)) (c :: v) q with a =char c
lookup-to-context (Node x ((a , E') :: x₃)) (c :: v) q | tt = lookup-to-context E' v q
lookup-to-context (Node x ((a , E') :: x₃)) (c :: v) q | ff = {!!}

-}
