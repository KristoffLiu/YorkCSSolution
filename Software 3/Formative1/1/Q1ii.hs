module Q1ii where -- 1 mark

{-

Consider the type:
-}
type Predicate a = a -> Bool
{-

Write a function `isAlphabet` that takes a string and returns `True` if the string has only Latin alphabetic characters (as used for standard English) and `False` otherwise.

Your solution should satisfy:

-}
testAl :: Bool
testAl = (isAlphabet "" == True ) &&
 (isAlphabet "hello!" == False) &&
 (isAlphabet "hello" == True) &&
 (isAlphabet "Hello" == True) &&
 (isAlphabet "SOF3" == False) &&
 (isAlphabet "Software" == True)


isAlphabet :: Predicate String
isAlphabet = all(`elem`(['a'..'z']++['A'..'Z']))