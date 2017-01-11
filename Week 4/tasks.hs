any' :: (a->Bool)-> [a] -> Bool
any' _ [] = False
any' f (x:xs) = if f x then True else any' f xs

all' _ [] = True
all' f (x:xs) = if f x then all' f xs else False

all1 _ [] = False
all1 f (x:xs) = if not (any' (not.f) xs) then True else False

split xs 0 = (xs,[])
split xs n = (take n xs, drop n xs)

--group' :: [a] -> [[a]]
--group' [] = []
--group' [a] = [[a]]
--group' (x:y:xs) = if x==y then [x: (group' (y:xs))] else [x]:[(group' (y:xs))]

minimumMINE :: (Ord a) => [a] -> a
minimumMINE [] = error "minimum of an empty list"
minimumMINE [x] = x
minimumMINE (x:xs)
	| x < minTemp = x
	| otherwise = minTemp
	where minTemp = minimumMINE xs
	
remove a [] = []
remove a (x:xs) = if (a==x) then xs else [x]++(remove a xs)

replicate' [] n = []
replicate' (x:xs) n
	| n<=0 = []
	|otherwise = x:(replicate' [x] (n-1))++(replicate' xs n)
	
sortMOE [] = []
sortMOE xs = (minimumMINE xs): (sortMOE (remove (minimumMINE xs) xs))

sortMoe = sortBy (<)