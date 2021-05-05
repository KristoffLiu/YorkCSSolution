module Q1i where -- 1 mark

{-

Consider the type:
-}
type Predicate a = a -> Bool
{-

Write a function `isPass` that takes a list of integers and returns
`True` if the sum is greater than or equal to 40 and `False` otherwise.

Your solution should satisfy:
-}
testP :: Bool 
testP = (isPass [] == False) &&
 (isPass [2, 5, 6, 7] == False) &&
 (isPass [12, 15, 16, 17] == True) &&
 (isPass [5, 8, 12, 15] == True)

isPass :: Predicate [Integer]
isPass = undefined