import Test.HUnit
-- Write a funciton that reverses a given list of integers
myReverse :: [Int] -> [Int]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++[x]
-- Solution goes here

-- Test cases
tests = test [ "Reverse an empty list" ~: "" ~: [] ~=? (myReverse []),
               "Reverse a list with only one element" ~: "" ~: [2] ~=? (myReverse [2]),
               "Reverse a list with more than one element" ~: "" ~: [101,100..1] ~=? (myReverse [1..101])]
