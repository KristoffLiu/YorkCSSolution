module Q2ii where
import Q2i
{-

Q2ii.[4 marks] A function tree is defined by:

tree f x = T x (map (tree f) (f x))

What is the polymorphic type of tree?
Describe its result in terms of its argu-ments.

-}

tree f x = T x (map (tree f) (f x))