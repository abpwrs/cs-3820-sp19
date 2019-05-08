module Inclass where

{- type for computations reading and write a single piece of state of type s,
   and producing an a -}
newtype Stateful s a = Stateful (s -> (s,a))

instance Functor (Stateful s) where
  fmap f (Stateful c) = Stateful (fmap (fmap f) c) -- wow: function type is a Functor instance, and so is pair

instance Applicative (Stateful s) where
  pure x = Stateful (\ s -> (s,x))
  (Stateful c1) <*> (Stateful c2) =
    Stateful (\ s ->
                let (s',f) = c1 s in
                let (s'',x) = c2 s' in
                  (s'',f x))

instance Monad (Stateful s) where
  (Stateful c1) >>= g =
    Stateful (\ s ->
                let (s',x) = c1 s in
                let (Stateful c2) = g x in
                  c2 s')

readState :: Stateful s s
readState = Stateful (\ s -> (s,s {- return the state as the output of the stateful computation -}))

writeState :: s -> Stateful s ()
writeState s' = Stateful (\s -> (s' {- throw away state s, use state s' -},
                                 () {- nothing interesting to return -}))

square x = x * x

squareIntState :: Stateful Int ()
squareIntState =
  readState >>= (writeState . square)

runStateful :: Stateful s a -> s -> (s,a)
runStateful (Stateful c) = c 
