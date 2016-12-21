import Test.HUnit
-- Write a function that takes a list of words and creates a sentence out of it.
-- Note the type declaration
sentence :: [String] -> String
sentence []     = []
sentence [x]    = x
sentence (x:xs) = x ++ " " ++ sentence xs
-- Solution goes here

-- Test cases
tests = test [ "Sentence made from an empty list" ~: "" ~: "" ~=? (sentence []),
               "Sentence with a couple of words in it" ~: "" ~: "I am a sentence!" ~=? (sentence ["I", "am", "a", "sentence!"])]
