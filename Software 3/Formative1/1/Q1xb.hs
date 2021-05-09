module Q1xb where -- 4 marks
import Q1xa

{-
Write a function `isTreeBal` which returns `True` when applied to a
balanced tree, and `False` otherwise. 
Remember, for a length-regular, balanced tree the value in a leaf is the length of the path from the root to the leaf.
You can _assume_ that the tree is length-regular when testing balance.

Your solution should satisfy: 
-}
testBal :: Bool
testBal = (isTreeBal (nullBR) == True ) &&
    (isTreeBal (Branch (Lf 1) 1 (Branch (Lf 2) 2 (Branch (Lf 3) 3 (Lf 3))))  == False ) &&
    (isTreeBal (Branch (Branch (Lf 2) 1 (Lf 2)) 2 (Branch (Lf 2) 3 (Lf 2))) == True ) 


isTreeBal :: BinTree a -> Bool 
isTreeBal (Lf x) = True
isTreeBal (Branch left node right) = 
    isTreeBal left
    && isTreeBal right
    && fromRoot left == fromRoot right
    where
        fromRoot (Lf x) = x
        fromRoot (Branch left node right) = max (fromRoot left) (fromRoot right)

