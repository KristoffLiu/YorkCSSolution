module Q2iii where
import Q2i
import Q2ii
{-
[3 marks] Draw a diagram of the tree represented by prune 2 ham,
where ham is defined by:

ham = tree (\r -> filter asc (map (:r) [2,3,5])) []
  where 
    asc (x:y:_) = x <= y
    asc = True

-}

-- draw diagram only