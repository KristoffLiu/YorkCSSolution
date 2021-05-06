module Q1vii where -- 4 marks

{-
Write a function `cmb :: Num a => String -> [a] -> [a] -> [a]`, which
returns a list as long as the shorter of the two input lists with all
odd-position elements taken from the first list and even-position
elements from the second list when the string is "List" or the product
of the numbers at the same position from the two lists when the string
is "Prod". any other string apart from "Prod" and "List" returns an
empty list.

Your solution should satisfy:
-}

testcmb :: Bool
testcmb = (cmb "Hello" [] [] == []) &&
 (cmb "Prod" [2, 3] [] == []) &&
 (cmb "List" [] [2, 3] == []) &&
 (cmb "Prod" [2, 3, 4] [5, 6] == [10,18]) &&
 (cmb "List" [2, 3, 4] [5, 6] == [5,2,6,3]) &&
 (cmb "Haskell" [2, 3, 4] [5, 6] == []) &&
 (cmb "Prod" [2, 3, 4] [5, 6, 7] == [10,18,28]) &&
 (cmb "List" [2, 3, 4] [5, 6, 7] == [5,2,6,3,7,4])


cmb :: Num a => String -> [a] -> [a] -> [a]
cmb str fs ss
    | str == "Prod" = cmbProd fs ss
    | str == "List" = cmbList fs ss
    | otherwise = []

cmbProd :: Num x => [x] -> [x] -> [x]
cmbProd fs ss = map multieach (zip fs ss)
    where
    multieach (x,y) = x*y

cmbList :: [a] -> [a] -> [a]
cmbList fs ss = foldr (++) [] (map tuple2list (zip ss fs))
    where
        tuple2list (x,y) = [x,y]
