import Test.HUnit
-- Write a function that, given a list and an index, returns the
-- element from the list at the given index
-- Note the type declaration: If there is no element at the given index,
-- return an appropriate value
myListRef :: [Int] -> Int -> Maybe Int
myListRef [] _     = Nothing
myListRef (x:xs) 0 = Just x
myListRef (_:xs) i = myListRef xs (i-1)

-- Test cases
tests = test [ "Try to get the third element of an empty list" ~: "" ~: Nothing ~=? (myListRef [] 2),
               "Get the second element of [2,1]" ~: "" ~: Just 13 ~=? (myListRef [2,13] 1),
               "Get the 42nd element of 1..100" ~: "" ~: Just 42 ~=? (myListRef [1..101] 41),
               "Try to get the -3rd element of a list" ~: "" ~: Nothing ~=? (myListRef [1,2,3] (-3))]
