import Test.HUnit
-- Write a function that duplicates every element in a given list.
-- Take a look at the tests for reference.

duplicate :: (Ord a) => [a] -> [a]
duplicate [] = []
duplicate [x] = x:x:[]
duplicate (x:xs) = x:x:duplicate xs

-- Test Cases
-- add more if you wish
tests = test [ "Test for value [1,2,3]" ~: "" ~: [1,1,2,2,3,3] ~=? (duplicate [1,2,3]),
               "Test for value [a]" ~: "" ~: ['a', 'a'] ~=? (duplicate ['a'])]
