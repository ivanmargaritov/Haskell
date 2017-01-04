map1 f l = [ f x | x <- l]

map2 f [] = []
map2 f (x:xs) =  f x : map2 f xs

filter1 f l = [ x | x <- l, f x]

filter2 f [] = []
filter2 f (x:xs) = if f x then x : filter2 f xs else filter2 f xs

zipWith1 f [] [] = []
zipWith1 f [] _ = []
zipWith1 f _ [] = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith f xs ys

twice f 0 = 0
twice f x = f (f x)

callNTimes 0 f x = x
callNTimes n f 0 = 0
callNTimes n f x = callNTimes (n-1) f (f x)

compose g f = (\ x -> g (f x))

lengthEncode1 [] = []
lengthEncode1 (x:xs) = [(x, length (filter (==x) (x:xs)))]++ lengthEncode1 (filter (/=x) xs)

lengthEncode2 [] = []
lengthEncode2 (x:xs) = show (length (takeWhile (==x) (x:xs))) ++ [x] ++lengthEncode2(dropWhile (==x) (x:xs))