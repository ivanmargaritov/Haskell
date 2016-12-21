-- Hey yo. Write a function that, given a string and a shift value,
-- returns a new string with all of its characters shifted by the given value.
-- Example:
-- caesarEncode "abc" 2 -> "cde"
-- caesarEncode "blah" 1 -> "cmbi"
-- What are you going to do if the shift value is greater than the alphabet length?
-- For the sake of simplicity, we will work only with lowercase letters from the
-- english alphabet

alphabet :: String
alphabet = ['a'..'z']

-- Declare and implement a function, named 'nextChar'
-- Given a character, return the next one.

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar a
	|a `elem` alphabet = succ a
	|otherwise = a

-- Declare and implement a function, named 'shiftCharBy'
-- Given a character and a shift value(n), get the n-th next character

shiftCharBy :: Char -> Int -> Char
shiftCharBy a 0 = a
shiftCharBy a n = shiftCharBy (nextChar a) (n-1) 

-- Declare and implement a function, named 'caesarEncode'
-- Given a string and a shift value, get the appropriate caesar encoded string

caesarEncode :: String -> Int -> String
caesarEncode a 0  = a
caesarEncode [] _ = []
caesarEncode a n  = [shiftCharBy c n | c <- a]
	
-- Declare and implement a function, named 'previousChar',
-- which is analogous to 'nextChar'

previousChar :: Char -> Char
previousChar 'a' = 'z'
previousChar a   = pred a

-- Declare and implement a more general version of 'shiftCarBy'
-- that works with negative shift values too

shiftCarBy :: Char -> Int -> Char
shiftCarBy a 0 = a 
shiftCarBy a n 
	| n < 0     = shiftCarBy (nextChar a) (25+n)
	| otherwise = shiftCarBy (nextChar a) (n-1)

-- Declare and implement a function, named 'caesarDecode',
-- that given a caesar-encoded string and its shift value, returns the original string

caesarDecode:: String -> Int -> String
caesarDecode a 0  = a
caesarDecode [] _ = []
caesarDecode a n  = [shiftCarBy c (-n) | c <- a]

-- Test the following calls to 'caesarDecode'
-- caesarDecode "Mmzzg kpzqabuia ivl pixxg vme gmiz! Mig bpm wlla jm mdmz qv gwcz nidwcz! Mig bpm nwzkm jm eqbp gwc! Ataw, uism aczm gwc omb dmzg dmzg lzcvs!!" 8
-- You know what Santa would cheer if he was black?
-- caesarDecode "pf pf pf!" 9
