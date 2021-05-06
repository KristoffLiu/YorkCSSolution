module RecurSion where

{-
# Kofi Appiah, 27 Dec 2020; Revised* 1/1/21


Recursion is a way of defining functions in which a function is applied inside its own definition, thus the function calls itself via self-referential expressions. In SOF1 a similar definition was provided that recursive definition is used in mathematical logic and computer science to define an object in terms of itself; here the object is a function. The notion that an object exhibits recursive behaviour if it can be defined by two properties will be used and later refined in a more elegant way.  
Recursion is an equally important concept in Haskell programming as it offers some level of concise and elegant solutions to problem. This is true in part, because Haskell uses nonstrict evaluation with higher-order functions that offer a more elegant solution. 
By defining a function recursively means to break down the problem at hand into smaller problems of the same kind and then
try to solve those sub-problems, breaking them down further if necessary. One of the sub-problems referred to as the _base case_, can't be broken any further and the programmer is expected to define it's solution explicitly. In Haskell computation is done by declaring _what_ something is rather than _how_ to compute it, making it different to most imperative languages. A Haskell program shouldn't just be a sequence of steps for the computer to execute, but rather directly defining what the desired result is; often in a recursive manner. 
To solve any problem recursively, first identify the base case and think about how to break the problem at hand into something similar but smaller. The focus here is to :
1. Explore what recursion is, 
2. Look at how recursive functions evaluate,
3. Go through the process of writing recursive functions and
4. Redefine some of the Prelude functions recursively. 

As part of the problem sets for Block 2, the function `fix` is used to compute fixed points of some other functions in a recursive way.

## Basic Concepts
There are a number of mathematical definitions which can easily be expressed using recursion. For example, the _Fibonacci sequence_ is such that each number is the sum of the preceding two numbers, starting from `0` and `1`. If `F(x) = F(x-1) + F(x-2)`, then `F(2)= F(1) + F(0)`. What of `F(5)`? Take the following steps to evaluate `F(5)`:

```haskell
F(5) = F(4)                                + F(3)
     = (F(3)               + F(2))         + (F(2)        + F(1))
     = ((F(2)       +F(1)) + (F(1)+F(0)))  + ((F(1)+F(0)) + F(1))
     = (((F(1)+F(0))+F(1)) + (F(1)+F(0)))  + ((F(1)+F(0)) + F(1))
     = (((1   +0))  +1)    + (1   +0)      + ((1   +0)    + 1)
     = 5
```

Now, let's write a Haskell expression to evaluate `F(5)`:
-}

fibFive :: Integer
fibFive = 3 + 2

{-

`fibFive` will return the correct result, but it only covers one possible result for the Fibonacci sequence. If the requirement is to have a function such that, given a number `n` as input it evaluates `F(n)`: the *n-th* member of the Fibonacci sequence, then `fibFive` is not ideal. To understand and establish the essence of a _base case_, evaluate the following expression in an attempt to define something more general for the Fibonacci sequence of numbers. 

-}

rushedFib :: Integer  -> Integer
rushedFib    n        =  rushedFib (n-1) +  rushedFib (n-2) 

{-

Applying `rushedFib` to `5` or any other number would never stop and result in a stack overflow. Even though the type system prevents calling with more than necessary        arguments.  Infinite calculation happens because the recursive arguments, incorrectly, become arbitrarily negative. To avoid stack overflow and hence stop a recursive expression from calling itself with more than necessary arguments, use a base case. Understanding the use of base case is critical for writing functions which are correct and terminate properly. A modified version of the generalised function for generating members of the Fibonacci sequence of numbers is described as follows using pattern matching to include the base case:

-}

goodFib :: Integer -> Integer
goodFib    0       =  0
goodFib    1       =  1
goodFib    n       =  goodFib (n-1) +  goodFib (n-2) 

{-

The base cases `goodFib 0 = 0` and `goodFib 1 = 1` in the improved version of the function offers the stopping condition and hence the evaluation of `goodFib 5` can be expanded as:

```haskell
F(5) = F(4)                                + F(3)
     = (F(3)               + F(2))         + (F(2)        + F(1))
     = ((F(2)       +F(1)) + (F(1)+F(0)))  + ((F(1)+F(0)) + F(1))
     = (((F(1)+F(0))+F(1)) + (F(1)+F(0)))  + ((F(1)+F(0)) + F(1))
```

Compared to _higher-order functions_ where the results of applying the first function gets passed as an argument to the next function, recursive functions have similar behaviour, but the first result gets passed back to the same function rather than a different one until the base case is reached.

Many functions can naturally be defined in terms of other functions. A typical example is the factorial of a positive integer *(n)*; the product of all numbers from `1` to *n*, which can be defined using the `product` function in Prelude with a generator like: 

```haskell
libFac :: Integer -> Integer
libFac n = product [1..n]
```

The `product` can also be defined in terms of the *left fold* `foldl` function. Folds functions will be covered in more details in Block 2. 
The factorial function can also be described recursively. Just as the Fibonacci sequence of numbers, a base case is required to define the function recursively. Using the factorial of zero as the base case, the recursive definition of the factorial function is:

-}

recFac :: Integer -> Integer
recFac    0       = 1
recFac    n       = n * recFac (n - 1)

{-

Applying `recFac` to `4` will result in the following:
```haskell
4 * (recFac (4 - 1))
4 * (recFac 3)
4 * (3 * (recFac (3 - 1)))
4 * (3 * (recFac 2))
4 * (3 * (2 * (recFac (2 - 1))))
4 * (3 * (2 * (1 * (recFac (1 - 1)))))
4 * (3 * (2 * (1 * (recFac 0))))
4 * (3 * (2 * (1 * 1)))
```
Applying `*` in the last expressing result in `24`; which is the factorial of `4`. As the base case is the identity value for multiplication, applying `recFac` to it doesn't change the result of previous applications. 

## Recursive Functions with multiple arguments
Having demonstrated that functions with a single argument can be defined recursively, the same concept will be extended to cover function with multiple arguments. The function `myReplicate` takes a positive integer and a value as arguments, and returns a list with the value repeated as many times as specified by the non-negative integer. 

### *replicate*
To define `myReplicate` recursively, a base case is needed just as with the definition of a single-argument function recursively. To replicate a value zero or less times, it should return an empty list. 

In general, a list with `n` repetitions of `x` is a list with `x` as its head and a tail consisting of `x` replicated `n-1` times, and that will be the recursive call in the definition of `myReplicate` as follows:

-}

myReplicate :: Integer -> a -> [a]
myReplicate 0 x = []
myReplicate n x = x: myReplicate (n-1) x

{-

Again pattern matching has been used, however guards can also be used to define a similar function that will take any integer rather than non-negative integers as one of the arguments. Applying `myReplicate 0 2` returns a empty list `[]` and `myReplicate 6 4` returns a list of six 4s `[4, 4, 4, 4, 4, 4]`. 
**Note** the function `myReplicate` as written can be applied to negative integers; in which case it runs until it overflows the stacks. 

## Recursion on lists

Recursion can also be used to define functions on lists. Understanding how lists are constructed will be useful when defining functions on lists recursively. Lists in Haskell are actually constructed one element at a time using the cons operator `:`, thus the list `[2, 4, 6]` is just and abbreviation for `2:(4:(6:[]))`. 

### *reverse*
A Prelude function which takes a list and returns the same list in reverse `reverse` can be defined recursively. The base case is to return the empty list `[]` when the function is applied `[]`. The reverse of a non-empty list can be defined recursively as the reverse of the tail, with the head stuck at the end. The recursive form of the `reverse` function can be defined as:

-}

recRev :: [a]    -> [a]
recRev    []     = []
recRev    (x:xs) = recRev xs ++ [x]

{-

It will become clear in Block 2 that there are better definitions for these Prelude functions. The definitions here have been simplified to explain recursion and it will become more obvious after the introduction of `Foldable`.

### *length*
`length` is another Prelude function which takes a list as argument and return the number of elements in the list. This can be defined recursively and the base case is to return zero when the function is applied the empty list. When applied to a non-empty list, the function returns the successor of the length of tail. The function `lenght` in it's recursive form can be defined as:

-}

recLen :: [a] -> Integer
recLen    []   = 0
recLen    (_:xs) = 1 + recLen xs

{-

The `recLen` function uses pattern matching and the second pattern has the wildcard `_` to represent the head of the list as it is not use in the body of the function and hence considered as a don't care. 

### *maximum*
The Prelude function `maximum`, which takes a list of things that can be ordered (e.g. integers - instance of the `Ord` type class) and returns the largest of them, can be defined recursively. As always, the base case should be defined: the maximum of a single-element list (singleton) is simply the element. Generally, when the list has more than one element, then the first element of the list (the head) can be compared with the maximum of the rest of the list (the tail), to find the biggest of the pair. 

This is a recursive definition of the `maximum` function using the Prelude function `max`:

-}

recmax :: (Ord a) => [a] -> a
recmax [] = error "Attempt to find the maximum of an empty list."
recmax [x] = x
recmax (x:xs) = max x (recmax xs)

{-
The number of times pattern matching has been used when defining recursive functions show how important they are. 

### *take*
Another Prelude function which takes a value and a list as arguments and returns a specified number (based on the value) of elements from the list is the `take`. The `take` function can be defined recursively using patterns as well as guards, when there is a Boolean condition test. The recursive definition of `take` will require two base cases: the first should return an empty list `[]` when the value is less than or equal to zero and the second also returns an empty list when the list argument to the function is empty. 

The recursive definition of the Prelude function `take` is as follows:

-}

recTake :: (Num i, Ord i) => i -> [a] -> [a]
recTake n _
      | n <= 0 = []
recTake _ [] = []
recTake n (x:xs) = x : recTake (n-1) xs

{-

The first pattern uses the `_` wildcard to match the list value because it is not used and hence it doesn't really matter. Also, a guard is used without an `otherwise` part, so that if the pattern fails it can fall through to the next pattern. The third pattern breaks the list into a head (`x`) and a tail (`xs`) and creates a list with `x` as the first element and `n-1` elements from `xs` as its remaining elements. 

### *zip*
The Prelude function `zip` takes two lists and returns a list of pairs; it truncate the longer list to match the length of the shorter one. Defining the `zip` function recursively is interesting in two ways, as it takes multiple arguments both of which are lists. Because each of the two arguments can be `[]`, the recursive function would require two base cases, one for each argument and returns an empty list in both cases. When both lists are non-empty, the heads of the two lists should be paired and append it to the `zip` of their tails. The recursive function can be defined as follows:

-}

recZip :: [a] -> [b] -> [(a,b)]
recZip    _      []  =  []
recZip    []     _   =  []
recZip (x:xs) (y:ys) = (x,y): recZip xs ys

{-

For example if `recZip` is applied to ['S', 'O', 'T', '3']  and "Haskell" `recZip ['S', 'O', 'T', '3'] "Haskell"`, it returns `[('S','H'),('O','a'),('T','s'),('3','k')]`. After four recursive calls, the function will try `recZip [] "ell"`, which matches the second pattern and returns `[]`. 

## Thinking recursively with *quicksort*
The problem of sorting a list containing elements like numbers that can be ordered, can be looked at recursively. There are a number of sorting algorithms (e.g. merge-sort and quick-sort), the recursive nature of defining a sorting algorithm will be evaluated here with optimisation in mind.   
Given a list, select the first element (also referred to as the pivot) and put all elements of the list less than the first on the left side. Then all elements equal to the first element are put together with elements greater than the first put on the right side. This is followed by recursively sorting all the elements that are on the left and right sides of the first element by calling the same function on them. The famous definition (`pseudoqs` below) is **not** quicksort.

-}

pseudoqs :: (Ord a) => [a] -> [a]
pseudoqs []     = []
pseudoqs (x:xs) =    pseudoqs [y | y <- xs, y < x]
                  ++ (x:)     [y | y <- xs, y == x]
                  ++ pseudoqs [y | y <- xs, y > x]

{-
It is not quicksort because it makes 3 passes through the list. All the elements less than the pivot are retrieved using the list comprehension `[y | y <- xs, y < x]`; a topic that will be covered more extensively in Block 2. The list comprehension will draw from `xs` and keep only those that satisfy the condition `y < x`. The list of elements larger than `x` are drawn in a similar way. The list concatenation operator (`++`) and a recursive application of the `pseudoqs` function is used to express that the final list is made up of a sorted list of elements less than the first element, followed by all elements equal to the first element and finally a sorted list of all elements greater than the first element. 

Here is a version that makes one pass.  It uses Dijkstra's Dutch National Flag algorithm (an algorithm worth looking at if you have not come across it before) to split the list into three, using accumulating parameters.
In the definition of the function `dnf''`, `dnf'` is called recursively to split the list into three; `less` the list of elements less than `x`, `eql` the list of elements equal to `x` and `more` the list of elements greater than `x`.

-}

dnf'' :: (Ord a) => a -> [a] -> ([a], [a], [a])
dnf'' x = dnf' [] [] [] 
  where
    dnf' less eql more []     = (less, eql, more)
    dnf' less eql more (y:ys) | y<x       = dnf' (y:less) eql more ys
                              | y==x      = dnf' less (y:eql) more ys
                              | otherwise = dnf' less eql (y:more) ys


{-
       But expressing the DNF algorithm using `foldr` (or even `foldl`, left as an exercise) is even better! **Folds** (`foldr`, `foldl`) can be used to implement any function where you traverse a list once, element by element, and then return something based on that. A fold takes three arguments: a function, a starting or base value and a list. With `foldr`, the function `addtotriple` and a starting value of ([], [], []) is used in the definition of the function `dnf`. Thus with `foldr`, the function `addtotriple x` adds a value `y` to the appropriate member of the triple `(less, eql, more)` depending on if it is less than, equal to or more than `x`, so folding `addtotriple x`, with starting value `([], [], [])`, over a list divides the list into the three sublists we need for quick sort in a **single** pass.

-}


addtotriple :: (Ord a) => a -> a -> ([a], [a], [a]) -> ([a], [a], [a])
addtotriple x y (ls, es, ms) | y < x     = (y:ls, es, ms)
                             | y == x    = (ls, y:es, ms)
                             | otherwise = (ls, es, y:ms)

dnf :: (Foldable t, Ord a) => a -> t a -> ([a], [a], [a])
dnf x = foldr (addtotriple x) ([], [], [])

realqs :: (Ord a) => [a] -> [a]
realqs [] = []
realqs (x:xs) = realqs less ++ (x:eql) ++ realqs more
  where
    (less, eql, more) = dnf x xs

