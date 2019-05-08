descendings :: (Ord a) => [a] -> [[a]]
descendings [] = []
descendings (x:xs) = (x:ys) : descendings zs
	where (ys, zs) = span (< x) xs
