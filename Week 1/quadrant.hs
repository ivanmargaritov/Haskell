import Test.HUnit
-- Write a function that takes a point (a tuple of integers)
-- and returns the quadrant to which it belongs (in a cartesian coordinate system)
-- (0,0), (0,y), (x,0) do not belong to any quadrant

quadrant :: (Int, Int) -> Maybe Int
quadrant (x,y)
	| x>0 && y>0 = Just 1
	| x<0 && y>0 = Just 2
	| x<0 && y<0 = Just 3
	| x>0 && y<0 = Just 4
	| otherwise  = Nothing
-- Solution goes here

-- Test Cases
tests = test [ "Check (0,0)" ~: "" ~: Nothing ~=? (quadrant (0,0)),
               "Check for first quadrant" ~: "" ~: Just 1 ~=? (quadrant (12,13)),
               "Check for second quadrant" ~: "" ~: Just 2 ~=? (quadrant (-12, 5)),
               "Check for third quadrant" ~: "" ~: Just 3 ~=? (quadrant (-5, -5)),
               "Check for fourth quadrant" ~: "" ~: Just 4 ~=? (quadrant (5,-12)),
               "Check (0,y)" ~: "" ~: Nothing ~=? (quadrant (0, 2)),
               "Check (x,0)" ~: "" ~: Nothing ~=? (quadrant (2,0))]
