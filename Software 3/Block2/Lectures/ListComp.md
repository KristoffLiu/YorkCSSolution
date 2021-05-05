```haskell
module ListComp where

```

# Kofi Appiah, 04 Jan 2021 

# Lists in Haskell 


### Basics

Some of the commonly used data structures in Haskell to gather several data values together are tuples and lists. A tuple has sequence of data values, separated by commas, inside parentheses. Tuples can have any number of components; when there are *n* components it is referred to as *n-tuple*, with two components normally referred to as a pair. A *0-tuple* or an empty tuple is written as `()`. Tuples have fixed number of components and there are no restrictions on the types of any of the components. A list is written as a sequence of elements, separated by commas and surrounded by square brackets `[]`.  A list compared to a tuple may have any number of elements, however in contrast to a tuple, all the elements **must** have the same type. For example, a list of integers or a list of characters is allowed but not a list of both integers and characters. The type of a list is written as **[a]**, where **a** is the element type. A **String** is a list of character and hence the constant `"sof3"` is exactly equivalent to writing `['s', 'o', 'f', '3']`. A list expression may have an expression which is evaluated, e.g. `[3*4, 7-2, 3, 6]` evaluates to `[12, 5, 3, 6]`.  The notation `..` can be used to fill in the missing values in a list. For example `[0..7]` means `[0, 1, 2, 3, 4, 5, 6, 7]`  and `[2, 5..11]`, counts up by `3` and evaluates to `[2, 5, 8, 11]`. Guess what the following will evaluate to and test them to verify:

```haskell
['b'..'k']
[12, 10..(-6)]
['1'..'9']

```

The `:` or *cons* operator is used to add an element to the front of a list. For example `2:[4, 6]` evaluates to `[2, 4, 6]`. The function `(:)` takes a value and a list and inserts the value to the front of the list; use `:t (:)` to check the type of the function and evaluate `(:) 's' "of3"`. Every list is built up with the `(:)` operator, starting from the empty list `[]`. The notational convention of a list like `3 : (4 : (5 : [])) = [3, 4, 5]` makes it easier to write expression involving the *head* and *tail* of a list. The parentheses can be omitted as `(:)` associates to the right and `3:4:5:[]` will be evaluated as follows:

```haskell
3:4:5:[]
3:4:[5]
3:[4, 5]
[3, 4, 5]
```


Another useful list  operator is the concatenation or `++` operator, used to put two lists together. For example `[2, 4, 6] ++ [8, 10, 12]` will evaluate to `[2, 4, 6, 8, 10, 12]` and Haskell traverse the entire list on the left (`[2, 4, 6]`); which may be inefficient if the left list is very long. Compared to the `:` operator which takes a single item and a list, the `++` operator takes two lists of the same type. Elements of the list can be accessed using the `!!` operator. As with many programming languages, the indices start at `0`, hence `[4, 3, 2, 6, 9] !! 4` evaluates to `9`.

### List Manipulation

The standard Prelude defines dozens of functions for dealing with lists, and they are useful to know to avoid wasting time reinventing simple functions already available. 

The `length` function takes a list and returns the number of elements in the list, e.g. the following evaluated to `10`:

```haskell
length "Software 3"
```

The `head` function takes a list and returns the first element, e.g. the following evaluates to `'S'`:

```haskell
head "Software 3"
```

The converse of the `head` function is the `tail` function which takes a list and returns all elements excluding the head of  the list. The following evaluates to `"oftware 3"`:

```haskell
tail "Software 3"
```

The `last` function takes a list and returns the very last element of the list. The following will evaluate to `'3'`:

```haskell
last "Software 3"
```

The converse of the `last` function is the `init` function, which takes a list and returns a list of all elements excluding the last element of the list. The following evaluates to `"Software "` :

```haskell
init "Software 3"
```

As an exercise, explore the behaviour of the above functions when applied to the empty list `[]`. The function `null` takes a list and returns `True` if the list is empty and `False` otherwise. 

The `reverse` function takes a list and returns a list with the elements reversed. Example the follow evaluates to `"3 erawtfoS"`:

```haskell
reverse "Software 3"
```

The `take` function takes a number and a list, and returns a list with the specified number of elements from the beginning of the input list. The following will evaluate to `"Soft"`:

```haskell
take 4 "Software 3"
```

In an attempt to `take` more elements than there are in the list, Haskell will just return the entire list.

The `drop` function in contrast to the `take` function takes a number and a list and drop at most the specified number of elements from the beginning of the list. For example the following will evaluate to `"ware 3"`:

```haskell
drop 4 "Software 3"
```

 The `maximum` function takes a list of items that can be **ordered** and return the largest element. The `minimum` function is similar to the `maximum` function but rather returns the smallest of the list of items in the list. 

The `elem` function takes an item and a list, and returns `True` if the item is in the list and `False` otherwise. The following example evaluates to `False`:

```haskell
elem '2' "Software 3"
```

Remember a function that takes two arguments like `elem` can be written as an *infix* operator like this:

```haskell
'2' `elem` "Software 3"
```

The function `elem` has a companion function, `notElem` which takes an item and a list, and returns `True` if the item isn't in the list and `False` otherwise. The two functions `all` and `any` take a predicate as first argument and `all` returns `True` if that predicate succeeds on every element of the list. The function `any` returns `True` if the predicate succeeds on at least one element of the list. For example `all even [2, 4, 6, 8]` returns `True` and `any odd [2, 4, 6, 8]` returns `False`. 

The function `zip` takes two lists and produces a list of pairs of their corresponding elements; this was first introduces in Block 1. `zip` can be used in conjunction with list comprehension to evaluate if a list is sorted or not. The function `adjpairs` takes a list and produce a list of all adjacent pairs, making it easier to compare pairs to know if they are in a particular order. For example `adjpairs [2, 4, 5, 6, 8]` evaluates to `[(2,4),(4,5),(5,6),(6,8)]`.

```haskell

adjpairs :: [a] -> [(a, a)]

adjpairs xs = zip xs $ tail xs

```

A function `issorted` can be defined using `adjpairs` and list comprehension to take a list and return `True` if the list is sorted and `False` otherwise.

```haskell

issorted :: Ord a => [a] -> Bool

issorted xs = and [x <= y | (x, y) <- adjpairs xs]

```

### List Comprehensions 

This is a simple but powerful syntax for generating a list from a list or lists without the need to write a program to build them. List comprehensions are based on the standard set comprehension notation in mathematics. The basic form of list comprehension is a list in which an expression appears first, followed by a vertical bar (read *such as*) and then a generator (a list or lists that gives the input for the comprehension), like: *[expression | generator]*.  The generator specifies a sequence of values that a variable takes on; this is written in the form *a <- list* and it means the variable `a` will take on each of the values in the list, one by one. For each of those values, the expression to the left of the bar is evaluated, and the result goes into the list. For example, the following list comprehension says that `a` should take on the values `3`, `4`, and `5`; for each of these the value of `2*a` goes into the result list, which is `[6, 8, 10]`:

```haskell
[2*a | a <- [3, 4, 5]]
```

The following will result in a list of the sum of tuples taken from the list `[(5, 7), (3, 6), (2, 8)]`, to give `[12, 9, 10]`.

```haskell
[a+b | (a, b) <- [(5, 7), (3, 6), (2, 8)]]
```

List comprehensions can have more than one generator, such that each value of the first generator taken by the first variable, the second variable gets all of the values in its generator. This may relate to a nested loop in a way where the first generator is the outer loop and the second generator is the inner loop. For example `[x ++ y | x <- ["SOF", "THE"], y <- ['1':[], '2':[], '3':[]]]` will generate `["SOF1", "SOF2", "SOF3", "THE1", "THE2", "THE3"]`. Remember `y` in the expression `x ++ y` should also be a list because the `++` operator takes two lists; the expression could also be written as `[x ++ y | x <- ["SOF", "THE"], y <- ["1", "2", "3"]]` because `"1"` is of type `[Char]`. In general when there are multiple generators in a list comprehension, the rightmost generator will be exhausted first, then the second rightmost, then the third rightmost and so on. 

#### Dependant Generators

When there are multiple generators, a later generator can depend on the variables that are introduced by earlier generators. For example this list comprehension `[(a, b*2) | a <- [1, 3, 4], b <- [a + 1..6] ]` evaluates to this list `[(1,4),(1,6),(1,8),(1,10),(1,12),(3,8),(3,10),(3,12),(4,10),(4,12)]` of all pairs of numbers `(a, b*2)`. This is similar to nested loops, where the inner loop can be defined on variables of the outer loop. In SOF1, you had the opportunity to write a Python program that would convert a list of lists into a list. That same problem can be solved in Haskell using list comprehension. 

```haskell

lists2list :: [[a]] -> [a]

lists2list xss = [x | xs <- xss, x <- xs]

```

For example `lists2list [[2, 4, 6], [7, 8, 9], [11, 12, 15]]` will evaluate to `[2,4,6,7,8,9,11,12,15]`. Note, there is a Prelude function `concat` that is similar to `lists2list`, but this is just to demonstrate how you can write concise code using function comprehension.

#### Predicates in List Comprehension 

Rather than using all elements of the generator list, it is also possible to be selective with the use of a *filter*. A filter is an expression of type **Bool**, which may use the generator variable. After a variable takes on a value of the generator, the filter is evaluated and if it is **False** then the value is left out. The filter can be seen as a **guard** to restrict the values produced by earlier generators. The following example will return the list `[15, 30, 45]` for all integers from `1` to `50` that are multiples of both `3` and `5`:

```haskell
[a | a <- [1..50], a `mod` 3 == 0 && a `mod` 5 == 0]
```

The following list comprehension will produce a list `[1, 3, 5, 15]` of all factors of `15`. It works by considering all combinations of `a` and `b` between `1` and `15`; the value of `a` is retained if the product is exactly `15`.

```haskell
[a | a <- [1..15], b <- [1..15], 15 == a*b]
```

Take a guess on the list that will be produced with `[b | a <- [1..15], b <- [1..15], 15 == a*b]` and test it afterwards. The following function `intfactors` uses list comprehension and when applied to a positive integer will return a list of all factors of the value applied.

```haskell

intfactors :: Integer -> [Integer]

intfactors a = [n | n <- [1..a], a ` mod ` n == 0]

```

The same function `intfactors` can then be used to determine if a positive integer is a prime number or not, by comparing the return list from `intfactors a` with the list `[1, a]`.

```haskell

intprime :: Integer -> Bool

intprime a = intfactors a == [1, a]

```

Conveniently, we can use list comprehension again with the function `intprime` to generate a list of all prime numbers less than a given positive integer value.  

```haskell

allprimes :: Integer -> [Integer]

allprimes a = [n | n <- [2..a], intprime n ]

```

Note, there are more efficient ways of computing prime numbers, but this is just to demonstrate how list comprehension can be used. A list comprehension with multiple generators can also have a filter, for example the following will return a list, which is the product of `x` and `y` greater than `50`:

```haskell
[x*y | x<-[8..12], y<-[5, 9], x*y>50 ]
```

One practical application of list comprehension is in database queries. For example if **studRec** is a database represented as a list of tuples and each tuple contains a student's name, their college, age and mark in SOF2 then, list comprehension can be used to return names and colleges of all students under the age of *20* who scored at least *67* in *SOF2*. 

```haskell
[name ++ " , " ++ college | (name, college, age, mark) <- studRec, age<20, mark>=67]
```

`studRec = [("Mark Foster", "Halifax", 19, 67), ("Clair Jones", "Goodricke", 19, 75), ("Beth May", "Constantine", 18, 68),("Jame Blankett", "Halifax", 20, 81) ...]` 



