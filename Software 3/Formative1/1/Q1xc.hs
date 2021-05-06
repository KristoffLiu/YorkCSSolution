module Q1xc where -- 2 marks
import Q1xa

{-
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
treeNodes (Lf n) = 0
treeNodes (Branch left node right) = 1 + treeNodes left + treeNodes right
