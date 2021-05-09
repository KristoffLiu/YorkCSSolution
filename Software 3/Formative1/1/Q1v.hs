module Q1v where -- 3 marks

{-
Write a function `sqDiff` that returns a list of squares of the 
difference between two consecutive numbers in a list, where the 
first number is greater than the second number in the list. 
Your solution should satisfy: 

-}

testsqDiff :: Bool 
testsqDiff =
    (sqDiff []               == []) &&
    (sqDiff [4, 6]           == []) &&
    (sqDiff [6, 4]           == [4]) &&
    (sqDiff [4, 6, 3, 1, 8]  == [9, 4])


sqDiff :: [Int] -> [Int]
sqDiff [] = []
sqDiff [x] = []
sqDiff (a:b:list)
    | a > b = (c * c) : sqDiff (b:list)
    | otherwise = sqDiff (b:list)
    where c = a - b


-- arithmitic expression 表达运算优先级
-- tuple 元组

