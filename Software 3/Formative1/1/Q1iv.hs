module Q1iv where -- 2 marks

{-
Given two lists over a `Num` type, `x`, return a single list whose length 
is the same as the minimum of the two and each element is the product 
of the numbers at the same position from the two lists. 

Your solution should satisfy: 
-}


testcmbProd :: Bool  
testcmbProd =
    (cmbProd [] [5, 6]  == []) &&
    (cmbProd [2, 3, 4] [5, 6]  == [10,18]) &&
    (cmbProd [0.23, 3.4, 7.88, 9.21] [3.4, 5] == [0.782,17.0]) &&
    (cmbProd [0.23, 3.4, 7.88, 2*0.3] [3.4, 1.3, 2.1, 2]  == 
        [0.782,4.42,16.548000000000002,1.2]) 

cmbProd :: Num x => [x] -> [x] -> [x]
cmbProd fs ss = map multieach (zip fs ss)
    where
    multieach (x,y) = x*y

-- input [2, 3, 4] [5, 6] 
-- zip -> [(2, 5), (3, 6)] 