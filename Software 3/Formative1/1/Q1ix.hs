module Q1ix where -- 4 marks


{-
Write a function `vowelDigit` which returns `True` when applied to a string with alternating vowels and digits and `False` otherwise. The vowel comes first in each pair and the input list must have even length.
Your solution should satisfy: 

-}

testvowelDigit :: Bool 
testvowelDigit =
    (vowelDigit ""         == False) &&
    (vowelDigit "a2"       == True) &&
    (vowelDigit "aa22"     == False) &&
    (vowelDigit "a2a2"     == True) &&
    (vowelDigit "b2b2"     == False) &&
    (vowelDigit "a2a21"    == False) &&
    (vowelDigit "2a4o"     == False) &&
    (vowelDigit "a2ab2"    == False) &&
    (vowelDigit "a2o5u8A0" == True) &&
    (vowelDigit "b2o5u8A0" == False)


vowelDigit :: String -> Bool  
vowelDigit "" = False
vowelDigit str =
    length str >= 2 
    && even(length str) 
    && and [(odd index && isVowel c) || (even index && isDigit c) | (c, index) <- zip str [1..]]
    where
        isVowel = (`elem` "aeiouAEIOU")
        isDigit = (`elem` "0123456789")

