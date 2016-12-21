import Test.HUnit
-- Write a function that takes the first n elements of a given list
myTake :: Int -> [Int] -> [Int]
myTake n _
	| n <= 0    = []
myTake _ []     = []
myTake n (x:xs) = x:myTake (n-1) xs
-- Solution goes here

-- Test cases
tests = test [ "Get something from an empty list" ~: "" ~: [] ~=? (myTake 23 []),
               "Get more elements than the list actually has" ~: "" ~: [1..111] ~=? (myTake 200 [1..111]),
               "Get a negative amount of elements" ~: "" ~: [] ~=? (myTake (-3) [1..101])]
