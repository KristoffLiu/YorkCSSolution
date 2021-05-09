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

    -- Maybe Int -> Nothing | Just 2

maybe2int :: [Maybe Int] -> Int 
maybe2int [] = 0
maybe2int [Just x] = x
maybe2int [Nothing] = 0
maybe2int (a:xs) = maybe2int [a] + maybe2int xs


maybe2int' :: [Maybe Int] -> Int 
maybe2int' xs = sum [x|Just x <- xs]

maybe2int'' :: [Maybe Int] -> Int 
maybe2int'' [] = 0
maybe2int'' (Just x:xs) = x + maybe2int xs
maybe2int'' (Nothing:xs) = 0 + maybe2int xs