strangeFunc [] = []
strangeFunc (x:[]) = [[x]]
strangeFunc (x:y:xs) = [x,y]:(strangeFunc xs)

strangeFunc1 [] = []
strangeFunc1 [x] = [[x]]
strangeFunc1 xs = [take 2 xs]++(strangeFunc1 (drop 2 xs))

intervalFunc ::Ord a => [(a,a)] -> (a,a)
intervalFunc xs = let minimumMine = map fst xs
                      maximumMine = map snd xs
                  in (minimum minimumMine, maximum maximumMine)
				  
intervalFunc1 xs = (foldl (\ acc x -> if fst x > acc then acc else fst x) (fst (head xs)) xs,
					foldl (\ acc x -> if snd x < acc then acc else snd x) (snd (head xs)) xs)
				  
pref [] = [[]]
pref xs = [take x xs | x <- [1..length xs]]

suff [] = [[]]
suff xs = [drop x xs | x <- [0..(length xs-1)]]

removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

sublists [] [] = False
sublists [] _ = False
sublists _ [] = False
sublists xs ys =
	let list1 = sort (removeDuplicates xs)
		list2 = sort (removeDuplicates ys)
	in 
		| if head list1 /= head list2 then False else True
		| otherwise = sublists (tail xs) (tail ys) 
		
	