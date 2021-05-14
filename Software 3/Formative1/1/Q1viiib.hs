module Q1viiib where -- 3 marks
import Q1viiia


{-

Write a function `tSOF1` that takes `y1Db` and return a list of tuples of the name, 
SYS1 mark and THE1 mark for all students who score more than 70 in SOF1.

Your solution should satisfy:
-}

testSOF1:: Bool
testSOF1 = tSOF1 s1Db == [("Lisa",65,60),("Mark",50,67)]

tSOF1 :: [CS1] -> [(String, Int, Int)]
tSOF1 ss = [(name s, sys1 s, the1 s) | s <- ss, sof1 s > 70]