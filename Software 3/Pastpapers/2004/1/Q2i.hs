module Q2i where
{-
Q2.A datatype for labelled trees is defined by

-}
 
data Tree a = T a [Tree a] deriving (Eq, Show)

{- 
where T x ts represents a tree with a root node labelled x and immediate 
subtrees represented by the items of ts. For example: 

                          0
                        / | \
                        1 2 5 
                         / \
                         3 4

-}

example :: Tree Int
example = T 0 [T 1 [], T 2 [T 3 [], T 4 []], T 5 []]

{-
Q2i.[5 marks] Define a function prune :: Int -> Tree a -> Tree a so that the result of prune n t,
for non-negative n, is a tree like t but with any nodes more than n generations from the root removed.
Outline the reduction of prune 1 example to its result T 0 [T 1 [] , T 2 [] , T 5 []].
-}

testprune :: Bool
testprune = prune 1 example == T 0 [T 1 [] , T 2 [] , T 5 []]

prune :: Int -> Tree a -> Tree a
prune 0 (T a treelist) = T a []
prune n (T a treelist) = T a [ prune (n-1) t | t <- treelist ]
