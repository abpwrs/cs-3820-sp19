module FuncExtSol where

import Data.Tree

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 f l = map <$> [f] <*> l

map3 :: (a -> b) -> [[[a]]] -> [[[b]]]
map3 f l = map2 <$> [f] <*> l

mapTree2 :: (a -> b) -> Tree (Tree a) -> Tree (Tree b)
mapTree2 f t = pure fmap <*> pure f <*> t

print2 :: Show a => a -> a -> IO ()
print2 a b = do print a; putChar '\n'; print b

fmap2 :: Functor f => (a -> b) -> f (f a) -> f (f b)
fmap2 f a = (fmap . fmap) f a

printShowables :: Show a => [a] -> IO ()
printShowables [] = return ()
printShowables (x:xs) = do putStrLn (show x); printShowables xs
