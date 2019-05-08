module Inclass where

data Expr = Num Float | Add Expr Expr | Mult Expr Expr | Div Expr Expr

{- like data EvalResult a =  ...
   except that can have only one constructor, and code will be specially
   optimized (to drop that constructor) when compiled by ghc. -}
newtype EvalResult a = EvalResult ([Float],Maybe a)

instance Functor EvalResult where
  fmap f (EvalResult (ds,m)) = EvalResult (ds,fmap f m)

instance Applicative EvalResult where
  pure x = EvalResult ([], pure x)
  (EvalResult (ds,mf)) <*> (EvalResult (ds',mx)) = EvalResult (ds ++ ds', mf <*> mx)

instance Monad EvalResult where
  (EvalResult (ds,Nothing)) >>= f = EvalResult (ds,Nothing)
  (EvalResult (ds,Just n)) >>= f = includeDivisors ds (f n)

instance Show a => Show (EvalResult a) where
  show (EvalResult (ds,Nothing)) = "divisors: " ++ show ds ++ "\nDivide by zero error."
  show (EvalResult (ds,Just n)) = "divisors: " ++ show ds ++ "\nResult is " ++ show n

evalFail :: EvalResult a
evalFail = EvalResult ([],Nothing)

includeDivisors :: [Float] -> EvalResult a -> EvalResult a
includeDivisors ds (EvalResult (ds',m)) = EvalResult (ds ++ ds',m)

-- divide by zero will cause Nothing to be returned
eval :: Expr -> EvalResult Float
eval (Num x) = pure x
eval (Add x y) = pure (+) <*> eval x <*> eval y
eval (Mult x y) = pure (*) <*> eval x <*> eval y
eval (Div x y) =
  do
    n <- eval y
    includeDivisors [n]
       (if n == 0 then  evalFail
        else pure (/) <*> eval x <*> pure n)

{- equivalent to:
  eval y >>= (\ n -> includeDivisors [n]
                       (if n == 0 then  evalFail
                        else pure (/) <*> eval x <*> pure n))
-}

{- before that was:
eval (Div x y) = case eval y of
                   EvalResult (ds,Nothing) -> evalFail ds
                   EvalResult (ds,Just 0) -> evalFail (0 : ds)
                   EvalResult (ds,Just n) -> pure (/) <*> eval x <*> includeDivisors [n] (pure n)
-}
test = (Div (Div (Num 3) (Num 4)) (Mult (Div (Num 3) (Num 3)) (Num 0)))
