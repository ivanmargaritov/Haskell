import Test.HUnit;
-- Hey yo! Write a function that calculates the x^n by given x and n.
-- Make sure it's fast.
-- x*x*x...*x n times is not fast.

--fastExpt :: (Integral a, Eq a) => a -> a -> a
fastExpt :: Int -> Int -> Int
fastExpt 0 _             = 0
fastExpt x 0             = 1
fastExpt x n
	| n `mod` 2 == 0 = fastExpt (x*x) (n `div` 2)
	| otherwise      = x * fastExpt x (n-1)

-- Test Cases
-- Add more if you wish
tests = test [ "2^3" ~: "" ~: 8 ~=? (fastExpt 2 3),
               "_^0" ~: "" ~: 1 ~=? (fastExpt 21392112 0),
               "4^3" ~: "" ~: 64 ~=? (fastExpt 4 3)]
