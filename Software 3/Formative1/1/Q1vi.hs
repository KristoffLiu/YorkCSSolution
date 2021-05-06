module Q1vi where -- 3 marks

{-
Write a function `maybe2int:: [Maybe Int] -> Int`, which returns the sum of all the integers in the list.
The presence of `Nothing` values does not affect the result.

Your solution should satisfy:
-}

testM2int :: Bool
testM2int = 
    (maybe2int []        == 0) &&
    (maybe2int [Just 23] == 23) &&
    (maybe2int [Nothing] == 0) &&
    (maybe2int [Just 2, Nothing, Just 3, Just 16, Nothing] == 21)


maybe2int :: [Maybe Int] -> Int 
maybe2int [] = 0
maybe2int ((Just a) : xs) = a + maybe2int (xs)
maybe2int ((Nothing) : xs) = maybe2int (xs)

