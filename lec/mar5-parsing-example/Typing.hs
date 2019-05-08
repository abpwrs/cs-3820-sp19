module Typing where

import Expr

checkh :: Term -> Type -> Type -> Bool
checkh Iden a b = a == b
checkh (Comp b s t) a c = checkh s a b && checkh t b c
checkh Unit a b = b == One

check :: Typing -> Bool
check (Typing t a b) = checkh t a b
