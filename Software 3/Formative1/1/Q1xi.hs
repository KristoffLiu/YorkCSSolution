module Q1xi where -- 5 marks

{-
Write a function `toBarcode` that takes a binary string (a string composed of 0s and 1s only)
as parameter and returns a string representing a bar-code.
The 0s are transformed into '.' and 1s into '|'.
In addition, the function must return Nothing if the string contains a character that is not a 0 or a 1.
You can also assume that a string (including an empty string) is always given.

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

