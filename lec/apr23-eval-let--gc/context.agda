module context where

open import lib
open import tp

context : Set
context = trie Tp -- map strings to Tps

empty-context : context
empty-context = empty-trie

context-lookup : context → string → maybe Tp
context-lookup = trie-lookup

context-insert : context → string → Tp → context
context-insert = trie-insert
