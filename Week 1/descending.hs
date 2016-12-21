import Test.HUnit

-- Write a function that checks whether the elements in a given list of integers
-- are in descending order
-- Pattern matching will help A LOT
descending :: [Int] -> Bool
descending []       = True
descending (x:[])   = True
descending (x:y:[]) = x>y
descending (x:y:xs)
		| x > y     = descending (y:xs)
		| otherwise = False
-- Solution goes here

-- Test cases
tests = test [ "Empty list" ~: "" ~: True ~=? (descending []),
               "A singleton list" ~: "" ~: True ~=? (descending [2]),
               "A list with two elements" ~: "" ~: True ~=? (descending [2,1]),
               "A list in ascending order" ~: "" ~: False ~=? (descending [1,2,3]),
               "A bigger list in descending order" ~: "" ~: True ~=? (descending [101,100..1])]
