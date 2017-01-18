remove a [] = []
remove a (x:xs) = if (a==x) then xs else [x]++(remove a xs)

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xs = concatMap (\x -> map (x:) $ permute $ remove x xs) xs

perms [] = [[]]
perms xs = [ x:ps | x <- xs , ps <- perms ( remove x xs ) ]

flip' f x y = f y x


powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xss ++ map (x:) xss
	where xss = powerset xs
	
