module Q2iv where
import Q2i
import Q2ii
{-
Q2iv. [6 marks] The function foldTree is defined by: 

foldTree :: (a -> [b] -> b) -> Tree a -> b 
foldTree f (T x ts) = f x (map (foldTree f) ts)

Give concise but informal specifications, including an illustrative example,
for each of the following functions:

f1 = foldTree T
f2 = foldTree (\x ys -> 1 + sum ys)
f3 = foldTree (T . product)

(Hint: if 1 and f 2 are polymorphic in the label type,
but f 3 can be applied only to trees with a specific type of label.) 
-}