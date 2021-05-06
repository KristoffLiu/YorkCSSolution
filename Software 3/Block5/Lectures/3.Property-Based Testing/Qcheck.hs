module Qcheck where
import Test.QuickCheck
import Data.Char

{-
# Property-Based Testing with QuickCheck.
# Kofi Appiah 17 April 2021
# Revised 23/4/21

## Getting QuickCheck [3]

You will need QuickCheck installed to be able `import Test.QuickCheck`. 
### Install QuickCheck in Linux - Ubuntu
```
sudo apt-get install hlint haskell-platform libghc-quickcheck2-dev
```
### Install QuickCheck in Linux - Fedora
```
sudo yum install ghc-QuickCheck-devel
```
### Using Cabal
1.	cabal update
2.	cabal install QuickCheck --lib

## Introduction

Up to this point, you have mainly tested your functions/code manually. Any time you load your code into `GHCi` to apply the functions to some values, you are testing your code manually. However, it is possible to automatically test your functions in Haskell through property-based testing. The tool **QuickCheck**[1], which can be used to quickly generate many tests will be discussed. Testing a program is a core part of the working programmer's toolkit, which plays a key role in maintaining the software quality as well as keeping the code on the straight-and-narrow path. In Haskell, the main testing mechanisms are the traditional unit testing and its more powerful descendant, the type-based "property" testing with a tool like QuickCheck (but there are others). Unit testing tests the smallest atomic units of a code independently of one another and does not necessarily verify that they all work together properly. 

Property testing was pioneered in Haskell and now adopted by other languages. It tests the formal properties without requiring formal proofs. A key aspect of property-based testing is to try the entire code with very many tests that would be infeasible to write by hand, mainly to uncover subtle edge cases that wouldn't be found otherwise. Property-based testing is useful in ensuring that the code meets the minimum requirements to satisfy laws, such as the laws of associativity or monads. Hence, when there are no assertable, truth-valued properties, property-based testing would not be an appropriate choice. 

In general, the Haskell compiler can be used to judge a well-typed code. However, a Haskell code can be well-typed and still not perform as expected or even have some runtime errors. That is where testing can be used to state an expectation, and then verify that the results of an operation meets that expectation. In summary, unit testing can be viewed as automating manual tests, while property testing is essentially automating unit tests.

## QuickCheck
QuickCheck is a tool which aids the Haskell programmer in formulating and testing properties of programs by describing properties as Haskell functions [1]. The functions can then be tested with randomly generated inputs based on the type. It is also possible to define custom test data for the property based functions. Properties are normally written in the same module as the functions they test, where they serve also as checkable documentation of the behaviour of the code. Test cases are randomly generated and the programmer can control the test data generation as discussed in [1]. 

As a first example, we will consider the function `isPalindrome` which returns true when applied to a string which reads the same backward as forward.
-}

isPalindrome :: String -> Bool
isPalindrome txt = txt == reverse txt

{-
The next meaningful thing is to test the function with a few examples (manually) to make sure it works as expected. Apply `isPalindrome` to words like "level", "kayak" and "refer", and expect it to return `True` in all the cases. If the specification demands that both "rotator" and "rotator!" should return true when `isPalindrome` is applied, then the function will have to be redefined or improved.

A simple but more specific fix to the "racecar" and "racecar!" problem is in `isPalindrome'`. But what happens if our next set of test cases include "rotator?"; as in 

```haskell
isPalindrome' "rotator?"
```
-}
isPalindrome' :: String -> Bool
isPalindrome' txt = alltxt == reverse alltxt
  where alltxt = filter (not . (`elem` ['!'])) txt
{-
Now to fully test `isPalindrome'`, it will be important to know how the function behaves with other punctuation. It is clear that the definition of `alltxt` can get very long as more punctuation marks get included and even harder to exhaustively test manually. In order to utilise QuickCheck's testing capabilities and hence the random test generator, a property will have to be defined for the function. In the case of `isPalindrome'`, the property can be a test for any string with or without punctuation. The expression `alltxt` can be refactored into a function like `freeFrom` to include more punctuation marks and the function `isPalindrome''` defined to use `freeFrom`. 
-}
freeFrom :: String -> String
freeFrom txt = filter (not . (`elem` ['!', '?', ';'])) txt

isPalindrome'' :: String -> Bool
isPalindrome'' txt = alltxt == reverse alltxt
  where alltxt = freeFrom txt

{-
The function to be really tested is `freeFrom`, which should be *punctuation invariant* and the function to express this will use `isPunctuation` from `Data.Char` as defined in `prop_pInvariant`. 
-}
prop_pInvariant :: String -> Bool
prop_pInvariant txt = freeFrom txt == filter (not . isPunctuation) txt

{-
In effect, the property defined in `prop_pInvariant` specifies that *applying `freeFrom` to any string `txt` should give the same answer as applying it to a string with no punctuation*. It makes it easier to have the property defined in code, so the QuickCheck library can be used to test it automatically. To use QuickCheck, call the `quickCheck` function on the property:

```haskell
quickCheck prop_pInvariant
```
The function `quickCheck` takes a property as parameter and applies it to a large number of randomly generated arguments (by default 100 but can be increased), it returns "OK" if the results is true in every case and reports counter-examples otherwise. The test at this point is bound to fail. A test output like the one below, shows that in passing values to the property, QuickCheck tried the `String` "%", which is a Unicode punctuation mark, which caused the property to fail.
``` haskell
*** Failed! Falsifiable (after 7 tests and 1 shrink):      
"%"
```
To see all the test cases generated by QuickCheck, call `verboseCheck` on the property:
```haskell
verboseCheck prop_pInvariant
```
In the case of `prop_pInvariant`, we test for equality with the simple `==` in Haskell and hence the function application returns `True` or `False`. If we are rather interested in returning the test data, the extensional equality `===` defined in QickCheck can be used as in `prop_pInvariant'`. In the case of `prop_pInvariant'`, a verboseCheck will return the evaluation of the actual terms. 
```haskell
verboseCheck prop_pInvariant'
```
Example verboseCheck prop_pInvariant may return:
Failed:                                        
"'\169"

verboseCheck prop_pInvariant' may return:
Failed:  
"'\128"
"'\128" /= "\128"

-}

prop_pInvariant' :: String -> Property
prop_pInvariant' txt = freeFrom txt === filter (not . isPunctuation) txt


{-

As a second example, the commutativity of addition defined in the function `comAddition` can be ascertained using QuickCheck, by defining the property `prop_Comaddition`. 
-}
comAddition :: Int -> Int -> Int
comAddition i j = i + j

prop_Comaddition :: Int -> Int -> Bool
prop_Comaddition x y = comAddition x y == comAddition y x

{-
Testing the property `prop_Comaddition` with QuickCheck should return:
```haskell
quickCheck prop_Comaddition
+++ OK, passed 100 tests.

```
showing that QuickCheck randomly generated 100 `Int` pairs and they all passed the test. 
The `Prelude` function `reverse`, satisfies a number of laws like:
* The reverse of a single item list is the same as the list : `reverse [x] == [x]`
* The reverse of the concatenation of two lists `xs ++ ys` is the same as the reverse of the second list `ys` concatenated with the reverse of the first list `xs`: `reverse (xs ++ ys) == reverse ys ++ reverse xs`
* The reverse of the reverse of a list is the same as the original list : `reverse (reverse xs) == xs`


Similar to the previous examples, to check the three laws of `reverse` above using QuickCheck, they will all have to defined using Haskell functions as:
-}
prop_revUnit x = reverse [x] == [x]

prop_revConcat xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_doubleRev xs = reverse (reverse xs) == xs

{-
Calling `quickCheck` on all the three properties above will pass. However, if the `prop_revConcat` property is defined as in `prop_revConcat'`, then the test would fail with a counter-example like 

```haskell
quickCheck prop_revConcat'
*** Failed! Falsifiable (after 4 tests and 6 shrinks):    
[0]
[1]

```
**Note:** QuickCheck uses the type to generate random test data, so the defined property for testing should have a type. In the case of a list, QuickCheck may use a number of `[]` if the type is not defined.
-}
prop_revConcat' :: [Int] -> [Int] -> Bool
prop_revConcat' xs ys = reverse (xs ++ ys) == (reverse xs) ++ (reverse ys) 

{-
The function `quickCheck` is actually overloaded, in order to be able to handle laws with varying number of types, and the overloading cannot be resolved if the law itself has a polymorphic type. Hence, the need to specify a fixed type at which the law is to be tested. 

Another interesting example is a test of *idempotency* (applying a function twice has the same effect as applying once) on a sort algorithm, just as for reverse `prop_doubleRev`. The function to test is `tSort`:
-}

tSort :: Ord a => [a] -> [a]
tSort []     = []
tSort (x:xs) = tSort lhs ++ [x] ++ tSort rhs
  where lhs = filter (< x) xs
        rhs = filter (>= x) xs
{-
As usual we can manually test `tSort` to ensure its robustness:
```haskell
tSort [21, 20..15]
tSort $ [21, 20..15] ++ [1, 3..9]
```

We can even test that the output of `tSort` is in fact sorted by defining a function `isOrdered` and a property `prop_tSort_Ordered`, which specifies that *applying `tSort` to a list of `Int` result in an ordered list*.
-}
isOrdered :: Ord a => [a] -> Bool
isOrdered xs = and (zipWith (<=) xs (tail xs))

prop_tSort_Ordered :: [Int] -> Bool
prop_tSort_Ordered xs = isOrdered (tSort xs)
{-
Calling `quickCheck` on the property `prop_tSort_Ordered` should return OK as shown:
​```haskell
quickCheck prop_tSort_Ordered
+++ OK, passed 100 tests.
```
The idempotency property can be written as a function `prop_idempotent`, which states an equality must hold for any input data that is sorted. 
-}

prop_idempotent:: [Int] -> Bool
prop_idempotent xs = tSort (tSort xs) == tSort xs

{-
The property defined in `prop_idempotent` can be tested manually as well as automatically using QuickCheck.

```haskell

*Qcheck> prop_idempotent []
True
*Qcheck> prop_idempotent [2, 4, -1, 0, 7, -8]
True
*Qcheck> prop_idempotent [6..29]
True
*Qcheck> quickCheck prop_idempotent
+++ OK, passed 100 tests.
*Qcheck> 
```

`prop_doubleRev'`, the alternative definition of `prop_doubleRev`; which is a definition of idempotence, could be used for both sorting and reversing.

-}

prop_doubleRev' :: [Int] -> Property
prop_doubleRev' xs = (reverse . reverse) xs === id xs

{-

Some properties (like "the head of a sorted list is the minimum value in the list"), hold under some specific conditions and QuickCheck provides an implication combinator to represent such conditional properties. In the case of testing the head of a sorted list to be the minimum element, the precondition is the list should not be empty and the property can be defined in Haskell as:

-}

prop_Sorted_min :: [Int] -> Property
prop_Sorted_min xs = not (null xs) ==> head (tSort xs) == minimum xs

{-
​
```haskell
quickCheck prop_Sorted_min
+++ OK, passed 100 tests.

```

The result type of the property `prop_Sorted_min` is not `Bool` but `Property`, because instead of checking the property for 100 random test cases, QuickCheck rather checks the property with 100 test cases that satisfies the condition `not (null xs)`. 
QuickCheck is able to formulate properties that quantify over functions as well. 

## Programmer-defined types and Arbitrary instances
Up to this point, it is clear that in QuickCheck test data is generated randomly based on the type. But how can QuickCheck generate random data for a programmer-defined type? QuickCheck introduces a type class `Arbitrary`, of which a type is an instance if one can generate arbitrary elements in it. QuickCheck relies on the type class called `Arbitrary`, a newtype called `Gen` for generating its random data and `arbitrary`, which is a value of type `Gen`. 

​```haskell
class Arbitrary a where
  arbitrary :: Gen a
```

`Gen a` is an abstract type representing a generator for type `a`. The type `Gen` is defined as 

```haskell
newtype Gen a = Gen (Rand -> a)

```

`Rand` is a random  number seed; a generator is just a function which can manufacture an `a` in a pseudo random way. `sample :: Show a => Gen a -> IO ()` from the `Test.QuickCheck` module can be used to see some of the random data. It is possible to specify your own data for generating `Gen` values. This is a function `alwaysSOF3`that will always return "SOF3" of type `String`.
-}

alwaysSOF3 :: Gen String
alwaysSOF3 = return "SOF3"

{-

QuickCheck defines test data generation via an instance of the class `Arbitrary` for each type and relies on the programmer to provide instances for the user-defined types. Combinators are provided to enable the programmer to easily produce generators. The simplest is `oneof`, which makes a choice among a list of alternative generators with a uniform distribution. For example, if the type `College` is defined by:

-}

data College = Alcuin | Halifax | Wentworth deriving Show

{-

Then, a suitable generator can be defined by:

-}

instance Arbitrary College where
 arbitrary = oneof [return Alcuin, return Halifax, return Wentworth]

{-

As another example, define a simple data type `Vector` that holds two `Int` values and how to display it:

-}

data Vector = TwoD Int Int

instance Show (Vector) where
  show (TwoD a b) = "<" ++ show a ++ "," ++ show b ++ ">"

{-

To use the `Vector` data type in QuickCheck, you need to provide an implementation of `Arbitrary` as:

-}
instance Arbitrary Vector where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ TwoD a b

{-

Just `a <- arbitrary` and `a <- arbitrary` in the class instance is enough to get the random `Ints`. Haskell will infer their types from the specification of `Vector` and will provide the appropriate arbitrary implementation, thus QuickCheck can now produce random Vectors with:

​```haskell

sample $ (arbitrary :: Gen Vector)

```

It is also possible to define a `Trail` as a list of `Vector`:

-}

newtype Trail = Trail [Vector] deriving Show

{-

In this case, the implementation of `Abitrary` that will allow the use of the `Trail` type in QuickCheck is:

-}

instance Arbitrary Trail where
  arbitrary = do
      list <- arbitrary
      return $ Trail list

{-

It is now possible for QuickCheck to produce random list of `Vector` using

```haskell

sample $ (arbitrary :: Gen Trail)

```

## References

[1]: Koen Claessen and John Hughes. 2011. QuickCheck: a lightweight tool for random testing of Haskell programs. SIGPLAN Not. 46, 4 (April 2011), 53–64. DOI:https://doi.org/10.1145/1988042.1988046
[2]: Bryan O'Sullivan, John Goerzen and Don Stewart. 2009. Real World Haskell. O'Reilly. 

[3]: https://hackage.haskell.org/package/QuickCheck

-}
