module Q1xa where -- 1 mark

{-
Consider
-}

data BinTree x = Lf Int | Branch (BinTree x) x (BinTree x) 
 deriving (Eq, Show)

{-
A tree is *balanced* if the length of the path from the root to the
deepest leaf on the left is the same as the right and each subtree has
the same structure. For a "Length-regular" tree, the value at a leaf is the length of the path to the leaf.  


Define `nullBR` to be the smallest possible balanced, length-regular tree.

-}
nullBR :: BinTree x 
nullBR = Lf 0