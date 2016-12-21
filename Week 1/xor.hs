import Test.HUnit
-- Implement the exclusive or (xor) function
xor :: Bool -> Bool -> Bool
xor x y
	| x == True && y == False = True
	| x == False && y == True = True
	| otherwise               = False
-- Solution goes here

-- Test cases
tests = test [ "Both arguments hold" ~: "" ~: False ~=? (True `xor` True),
               "Both arguments do not hold" ~: "" ~: False ~=? (False `xor` False),
               "First argument holds" ~: "" ~: True ~=? (True `xor` False),
               "Second argument holds" ~: "" ~: True ~=? (False `xor` True)]
