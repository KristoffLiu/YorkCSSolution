module Q1viiic where -- 3 marks
import Q1viiia


{-
Write a function `avgSYS1` that returns the average mark for SYS1. 

Your solution should satisfy:
-}

testavg:: Bool
testavg = avgSYS1 s1Db == 60.8

avgSYS1 :: [CS1] -> Float   
avgSYS1 ss = fromIntegral(sum list) / fromIntegral(length list)
    where list = [sys1 s | s <- ss]