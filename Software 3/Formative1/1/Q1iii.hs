module Q1iii where -- 2 marks

{-

Write a function `cmbList`, such that, given two lists, it returns a single list with all odd-position elements taken from the first list and even-position elements from the second list. Note, lists are 0-indexed.

Your solution should satisfy: 
-}

testcmbList :: Bool  
testcmbList =
    (cmbList ['s'] "comp"  == "cs") &&
    (cmbList "otae""sfwr" == "software") &&
    (cmbList [1, 3, 5] [0, 2, 4, 6]  == [0,1,2,3,4,5]) &&
    (cmbList ["1", "2", "3"]["THE", "SOF", "SYS"] == 
        ["THE","1","SOF","2","SYS","3"])     


cmbList :: [a] -> [a] -> [a]
cmbList fs ss = foldr (++) [] (map tuple2list (zip ss fs))
    where
        tuple2list (x,y) = [x,y]

-- (1,0)--tuple 元组

-- map 映射
--     fx 数组
-- map 映射
--     fx value 数组