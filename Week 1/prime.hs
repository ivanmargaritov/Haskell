import Test.HUnit
-- Write a function that checks whether given integer is prime
-- Neither 0 nor 1 are prime
prime :: Int -> Bool
prime number = null [x | x <- [2..(number-1)], number `mod` x == 0]
	

-- Test Cases
tests = test [ "Test for value 2" ~: "" ~: True ~=? (prime 2),
               "Test for value 6" ~: "" ~: False ~=? (prime 6),
               "Test for value 19231283912" ~: "" ~: False ~=? (prime 19231283912)]
