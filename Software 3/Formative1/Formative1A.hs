module Formative1A where
-- updated 9/3/21 formative 1 2021
{-
Consider the type:
-}
type Predicate a = a -> Bool
{-

## PA_Qi: [1 mark] [1]
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
isPass ns = sum ns >= 40

-- alternative solution

isPass' :: Predicate [Integer]
isPass' = (>=40) . sum

{-
## PA_Qii: [1 mark] [2]
Write a function `isAlphabet` that takes a string and returns `True`
if the string has only Latin alphabetic characters (as used for standard English) and `False` otherwise.

Your solution should satisfy:
-}
testAl :: Bool
testAl = (isAlphabet "" == True ) &&
 (isAlphabet "hello!" == False) &&
 (isAlphabet "hello" == True) &&
 (isAlphabet "Hello" == True) &&
 (isAlphabet "SOF3" == False) &&
 (isAlphabet "Software" == True)


isAlphabet:: Predicate String 
isAlphabet = all (`elem` (['A'..'Z']++['a'..'z']))


{-

## PA_Qiii: [2 mark] [4]
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
cmbList fs ss = foldr (++) [] (map tup2lst (zip ss fs))
         where
         tup2lst (x,y) = [x,y]
         
cmbList' :: [a] -> [a] -> [a]
cmbList' fs ss = concatMap tup2lst (zip ss fs)
         where
         tup2lst (x,y) = [x,y]

-- other possibilities
cmbList'', cmbList''' :: [a] -> [a] -> [a]
cmbList'' = (concat .) . zipWith (\x y->[y, x])

cmbList''' _  []     = []
cmbList''' os (e:es) = e : cmbList''' es os


{-
## PA_Qiv: [2 mark] [6]
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
cmbProd fs ss = foldr (++) [] (map tup2prod (zip fs ss))
         where
         tup2prod (a,b) = [a*b]

-- alternative and even shorter...
cmbProd' :: Num a => [a] -> [a] -> [a]
cmbProd' = zipWith (*)

{-
## PA_Qv: [3 mark] [9]
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

sqDiff []        = []
sqDiff xs@(_:ys) = [(a - b)^2 | (a, b) <- zip xs ys, a > b]

-- more efficient to not repeat expressions, and use an "as" pattern but also acceptable:
sqDiffL :: [Int] -> [Int]
sqDiffL    []     = []
sqDiffL    (x:xs) = [ (a - b) * (a - b) | (a,b) <- zip (x:xs) xs, a > b ]

sqDiff' :: [Int] -> [Int]
sqDiff' [] = []
sqDiff' [x] = []
sqDiff' (a:b:xs) 
               |a > b = (c * c): sqDiff' (b:xs)
               |otherwise = sqDiff' (b:xs)
               where c = a - b

sqDiff'' :: [Int] -> [Int]
sqDiff'' ns = foldr f [] (zip ns (tail ns))
  where
    f (a,b) | a>b       = (((a-b)^2) :)
            | otherwise = id

{-
## PA_Qvi: [3 mark] [12]
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
maybe2int ((Just x):xs) = x + maybe2int xs
maybe2int (Nothing:xs) = maybe2int xs

maybe2int' :: [Maybe Int] -> Int
maybe2int' xs = sum [x | (Just x) <-xs]

-- Later, you will realise that type can be generalised.
maybe2int'' :: (Functor f, Foldable f) => f(Maybe Int) -> Int
maybe2int'' = sum . fmap (maybe 0 id)

{-

## PA_Qvii: [4 mark] [16]

Write a function `cmb :: Num a => String -> [a] -> [a] -> [a]`, which
returns a list as long as the shorter of the two input lists with all
odd-position elements taken from the first list and even-position
elements from the second list when the string is "List" or the product
of the numbers at the same position from the two lists when the string
is "Prod". Any other string apart from "Prod" and "List" returns an
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
cmb "List" fs ss = concatMap tup2lst (zip ss fs)
           where
             tup2lst (x,y) = [x,y]
cmb "Prod" fs ss = concatMap tup2prod (zip ss fs)
           where
             tup2prod (a,b) = [a*b]
cmb _ _ _ = [] 

-- alternative solution
cmb' :: Num a => String -> [a] -> [a] -> [a]
cmb' cmd fs ss = concatMap (f cmd) (zip ss fs)
  where
    f "List" (x, y) = [x, y]
    f "Prod" (x, y) = [x * y]
    f _      _      = []

{-
## PA_Qviii: [8 mark] [24]
Consider the type of Stage 1 students record `CS1`:
-}

data CS1 = Student {name :: String, the1, sof1, sys1 :: Int}

{-

A section of records for five Stage 1 students are available in the database `s1Db`. Each record for a student has the student's name and marks for three modules (THE1, SOF1 and SYS1).

-}
s1Db :: [CS1]
s1Db = [Student {name = "Beth", the1 = 65, sof1 = 58, sys1 = 79},
       Student {name = "Adam", the1 = 55, sof1 = 68, sys1 = 61},
       Student {name = "Lisa", the1 = 60, sof1 = 72, sys1 = 65},
       Student {name = "Will", the1 = 71, sof1 = 52, sys1 = 49},
       Student {name = "Mark", the1 = 67, sof1 = 78, sys1 = 50}]

{-
### Qviiia: [2 mark]
Write a function `the1Mk` that takes any `CS1` records like `s1Db` and returns a list of pairs of the name and THE1 mark of each student in the same order as they appear in the database.

Your solution should satisfy:
-}

test1MK :: Bool
test1MK = the1Mk s1Db == [("Beth",65),("Adam",55),("Lisa",60),("Will",71),("Mark",67)]

the1Mk :: [CS1] -> [(String, Int)]

the1Mk ss = [(name s, the1 s) | s <- ss] 


[(name s, the1 s) | s <- ss]


{-
### Qviiib: [3 mark]
Write a function `tSOF1` that takes `s1Db` and return a list of tuples of the name, SYS1 mark and THE1 mark for all students who scored more than 70 in SOF1.

Your solution should satisfy:
-}

testSOF1:: Bool
testSOF1 = tSOF1 s1Db == [("Lisa",65,60),("Mark",50,67)]


tSOF1 :: [CS1] -> [(String, Int, Int)]
tSOF1 ss = [(name s, sys1 s, the1 s) | s <- ss, sof1 s > 70] 


{-

### Qviiic:[3 mark]
Write a function `avgSYS1` that returns the average mark for SYS1. 

Your solution should satisfy:
-}

testavg:: Bool
testavg = avgSYS1 s1Db == 60.8

avgSYS1 :: [CS1] -> Float   
avgSYS1 ss = fromIntegral (sum gList) / fromIntegral (length gList)
    where
      gList = [sys1 s | s <- ss] 

{-
## PA_Qix: [4 mark] [28]
Write a function `vowelDigit` which returns `True` when applied to a string with alternating vowels and digits and `False` otherwise. The vowel comes first in each pair and the input list must have even length.
Your solution should satisfy: 

-}

testvowelDigit :: Bool 
testvowelDigit =
    (vowelDigit ""         == False) && -- only the case for 2021 formative 1
    (vowelDigit "a2"       == True) &&
    (vowelDigit "a2a"      ==False) &&
    (vowelDigit "aa22"     == False) &&
    (vowelDigit "a2a2"     == True) &&
    (vowelDigit "b2b2"     == False) &&
    (vowelDigit "a2a21"    == False) &&
    (vowelDigit "2a4o"     == False) &&
    (vowelDigit "a2ab2"    == False) &&
    (vowelDigit "a2o5u8A0" == True) &&
    (vowelDigit "b2o5u8A0" == False)


vowelDigit :: String -> Bool 
vowelDigit cs = even (length cs) && length cs> 1  && and [(even p && isVowel c ) || (odd 
 p && isDigit c) | (c, p) <- zip cs [0..]]


isVowel :: Char -> Bool
isVowel       = (`elem` "aeiouAEIOU")

--alternative definitions for isVowel
--isVowel v = v `elem` "aeiouAEIOU"
--isVowel = flip elem "aeiouAEIOU"


isDigit :: Char -> Bool
isDigit n = n `elem` "0123456789"

vowelDigit' :: String -> Bool 
vowelDigit' cs =even (length cs) && length cs > 1 && vDcheck cs 0

vDcheck :: String -> Int -> Bool 
vDcheck [] n = True 
vDcheck (c:cs) n 
              | even n = isVowel c && vDcheck cs (n+1)
              | otherwise  = isDigit c && vDcheck cs (n+1)

vd :: String -> Bool -- this is only true for the 2021 Formative 1
vd [v, d] = isVowel v && isDigit d
vd (v:d:cs) = isVowel v && isDigit d && vd cs
vd _        = False
 

vd' :: String -> Bool
vd' = maybe False gg . fst . foldl ff (Just 0, True)
  where
    ff st@(Nothing, _) _ = st
    ff (Just n, b) c     = (next, not b)
      where
        next | (if b then isVowel else isDigit) c = Just (n+1)
             | otherwise                          = Nothing
    gg n = (n /= 0) && even n


{-
## PA_Qx: [7 mark] [35]
Consider

-}
data BinTree x = Lf Int | Branch (BinTree x) x (BinTree x) 
 deriving (Eq, Show)
{-
A tree is *balanced* if the length of the path from the root to the
deepest leaf on the left is the same as the right and each subtree has
the same structure. For a "Length-regular" tree, the value at a leaf is the length of the path to the leaf.  

### PA_Qxa: [1 mark]
Define `nullBR` to be the smallest possible balanced, length-regular tree.

-}
nullBR :: BinTree x 
nullBR = Lf 0


{-
### PA_Qxb: [4 mark]
Write a function `isTreeBal` which returns `True` when applied to a
balanced tree, and `False` otherwise. Remember, for a length-regular, balanced tree the value in a leaf is the length of the path from the root to the leaf. You can _assume_ that the tree is length-regular when testing balance.

Your solution should satisfy: 
-}
testBal :: Bool
testBal = (isTreeBal (nullBR) == True ) &&
    (isTreeBal (Branch (Lf 1) 1 (Branch (Lf 2) 2 (Branch (Lf 3) 3 (Lf 3))))  == False ) &&
    (isTreeBal (Branch (Branch (Lf 2) 1 (Lf 2)) 2 (Branch (Lf 2) 3 (Lf 2))) == True ) 


isTreeBal :: BinTree a -> Bool
isTreeBal (Lf n) = True
isTreeBal (Branch left y right) = isTreeBal left && isTreeBal right && (fromRoot left == fromRoot right)
    where
      fromRoot (Lf _) = 0
      fromRoot (Branch lf rn rt) = 1 + max (fromRoot lf) (fromRoot rt)


isTreeBal' :: BinTree a -> Bool 
isTreeBal' (Lf n) = True 
isTreeBal' (Branch left y right) = isTreeBal' left && isTreeBal' right && (fromRoot' left == fromRoot' right)
    where
      fromRoot' (Lf x) = x 
      fromRoot' (Branch lf rn rt) = max (fromRoot' lf) (fromRoot' rt)

{-
### PA_Qxc: [2 mark] 
Write a function `treeNodes` which returns the total number of interior nodes in a given tree. Note, the tree need not be balanced.

Your solution should satisfy: 
-}
testN :: Bool 
testN = 
    (treeNodes nullBR == 0) &&
    (treeNodes (Branch (Lf 1) 3 (Lf 1)) == 1) &&
    (treeNodes (Branch (Lf 1) 3 (Branch (Lf 2) 8 (Lf 2))) == 2) &&
    (treeNodes (Branch (Lf 1) 1 (Branch (Lf 2) 2 (Branch (Lf 3) 3 (Lf 3)))) == 3) &&
    (treeNodes (Branch (Branch (Lf 2) 1 (Lf 2)) 2 (Branch (Lf 2) 3 (Lf 2))) == 3)

treeNodes :: BinTree a -> Int
treeNodes (Lf i) = 0
treeNodes (Branch left y right) = 1 + treeNodes left + treeNodes right


{-
## Qxi:[5 mark] [40]
Write a function `toBarcode` that takes a binary string (a string composed of 0s and 1s only) as parameter and returns a string representing a bar-code. The 0s are transformed into '.' and 1s into '|'. In addition, the function must return Nothing if the string contains a character that is not a 0 or a 1. You can also assume that a string (including an empty string) is always given.

Your solution should satisfy:
-}

testBcode:: Bool
testBcode = 
  (toBarcode ""    == Just "") &&
  (toBarcode "00" == Just "..") &&
  (toBarcode "1111" == Just "||||") &&
  (toBarcode "0010111" == Just "..|.|||") &&
  (toBarcode "01120" == Nothing) &&
  (toBarcode " " == Nothing)

toBarcode :: String -> Maybe String  
toBarcode [] = Just [] 
toBarcode xs 
          | all bCheck xs = Just (map barEq xs)
          | otherwise = Nothing 
              where barEq a = if a=='1' then '|' else '.'

bCheck :: Char  -> Bool 
bCheck c = c `elem` "01"

-- Alternative solution using a fold:
toBarcode' :: String -> Maybe String
toBarcode' = foldr f (Just [])
  where
    f '0' (Just cs) = Just ('.':cs)
    f '1' (Just cs) = Just ('|':cs)
    f _   _         = Nothing


