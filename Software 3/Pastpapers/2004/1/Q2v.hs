module Q2iv where
import Q2i
import Q2ii
import Q2iv
{-
Q2v.[7 marks] Finally, consider a state-space search problem specified by three parameters:

goal :: State -> Bool
init :: State
succ :: State -> [State]

A solution to the problem is a list of states: the first must be init, the last must satisfy goal,
and for all consecutive states x, y the list succ x must contain y
Define a function

solve :: (State -> Bool) -> Tree State -> [State]

so that the result of solve goal ( tree init succ ) is a shortest solution.
(Note: you may assume that a solution exists and that the tree is finite;
any aux-iliary functions that do not appear elsewhere in this question must be defined in full and briefly explained.) 

-}