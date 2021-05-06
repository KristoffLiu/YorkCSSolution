```{.haskell}
module FuncDef where
```

Kofi Appiah, 09 Dec 2020, revised 10/01/21
==========================================

Functions are central to functional programming languages and hence
Haskell. We will look at functions in terms of: 
1. how to use them, 
2. their types, and 
3. how to define them. 

A function is an instruction for producing an output from an input or
argument. In Haskell, functions are constructed through syntactic means
of denoting that an expression takes arguments. All values including
functions in Haskell can be arguments to functions and are referred to
as first-class values. Syntax that enables writing Haskell functions in
a readable and sensible manner will be discussed, focusing on how to
quickly deconstruct values, avoid long *if...then..else* chains, and how
to define intermediate computations for reuse.

Function Application
--------------------

This is an expression which uses a function to perform computation, in
other words we say that a function is applied to its arguments. For
example, we might apply the function `triple` to the argument `2` and
obtain the result `6`. In terms of notation, one can naively describe a
function application as consisting of the function name, followed by a
space then the function argument or arguments. Where there are multiple
arguments, they are separated by spaces (and not commas) and must not
have parentheses around them. They need not have parentheses, unless to
ensure a correct parse. Strictly speaking, Haskell functions only have a
single argument, but they can return a function which is what consumes
the next argument.

These are some examples of function application in Haskell:

``` {.haskell}
sqrt 25.0
odd 13
even 13
compare 8 9
```

We can define a function which evaluates the following polynomial with
its two arguments: 
```{.haskell}
p1 :: Integer -> Integer -> Integer
p1 n c = 3*n^2 + 4*n + c
```

To evaluate the polynomial with `n <- 5` and `c <- 6` use:

``` {.haskell}
p1 5 6
```

Function Types
--------------

Just as data values have types, so do functions, and function types
describe the type of arguments as well as type of result the function
returns. A function that takes an argument of type `a` and returns a
result of type `b` has a function type written as `a -> b` and written
as `f :: a -> b`. For example, the `p1` function described above has the
type `Integer -> Integer -> Integer`; meaning it takes two integers and
return an integer (strictly speaking, the function takes an `Integer`
and returns a *function* of type `Integer -> Integer`). These are type
declarations of some Haskell functions:

``` {.haskel}
sqrt :: Double -> Double
max :: Ord a => a -> a -> a
not :: Bool -> Bool
toUpper :: Char -> Char
```

*Note: Haskell insists that a type declaration is always accompanied by
a definition.* `[Integer]` in Haskell means a list of integers and will
be discussed in another Block. We can define a function `oneto` that
returns the list of integers from one to a given limit as follows: 

```{.haskell}
oneto :: Integer -> [Integer]
oneto x = [1..x]
```

There is no restriction that functions must be total on their
argument type and hence there may be some arguments with no defined
result. For example the result of the library function tail is undefined
if the list is empty.

Operators and Functions
-----------------------

In Haskell, an *operator* is a function with exactly two arguments
written with the operator between the arguments. For example, the `+`
operator takes two numbers as arguments and add them, and it is written
between the numbers as `5+8`. Since an operator is a function, it has a
function type. As a function, any operator can be converted into a
curried function by enclosing the operator in parentheses before its
arguments, as in `(*) 4 5` to give `20`. When an operator is used by
itself, for example to state its type, it must be enclosed in
parentheses like:

``` {.haskell}
(*) :: Integer -> Integer -> Integer
(||) :: Bool -> Bool -> Bool
```

In general, an operator `#` for arguments `a` and `b` can be expressed
in the form `(#)`, `(a #)` or `(# b)`, referred to as sections. As
functions, the three expressions can be formalised using lambda
expressions as follows:

``` {.haskell}
1. (#) = \a -> (\b -> a # b)
2. (a #) = \b -> a # b
3. (# b) = \a -> a # b
```

A function defined in Haskell with two arguments can be converted into
an operator by enclosing the name of the function in single backquotes,
as in

``` {.haskell}
2 `max` 5
```

For example, the function `div` can be applied to two arguments in a
function application: `div 4 2` or use it as an operator like this:

``` {.haskell}
4 `div` 2
```

Thus, an operator can be treated as a prefix function by putting
parentheses around it, and a function can be treated as an operator by
putting single backquote characters around it. Test the function as an
operator with the `multi` function defined as follows:

``` {.haskell}
multi :: Integer -> Integer -> Integer
multi a b = a * b
```

Function Definitions
--------------------

A new function can be defined by giving the *type declaration* followed
by the *defining equation*. The type need not be given: Haskell deduces
the most general type. However, it is **GOOD PRACTICE** to give the
type, as a check.

The type declaration has the form:

``` {.haskell}
nameOfFunction :: argumenmtType1 -> ... -> argumenmtTypeN -> resultType
```

and the defining equation has the form:

``` {.haskell}
nameOfFunction argumenmt1 argumenmt2 ... argumenmtN = expression in terms of the arguments
```

*It is quite hard to state, but things can be more general than this.*

Even though functions can be written directly in `ghci`, it’s not a good
environment for doing so. It accepts only a highly restricted subset of
Haskell. Function definitions should be inserted into a Haskell script
file, and the defined function can be used once the file is loaded. For
example, the following function `triple` takes an Integer and triples
it. Here is the function definition:

```{.haskell}
triple :: Integer -> Integer
triple x = 3 * x
```
When a function is applied to an argument, the value of the argument is
available to the expression on the right-hand side of the function’s
defining equation. For example, if we evaluate `triple 6`, then the `x`
is instantiated to be `6`, and the right-hand side `3 * x` is evaluated,
producing the result `18`. The scope of the argument is just the
right-hand side of the defining equation.

Pattern Matching
----------------

Pattern matching is used to specify patterns to which some data should
conform and to deconstruct the data according to those patterns. It can
also be applied on a number of data type, including numbers, characters,
lists and tuples. Using the following `squares` function, the left-hand
side of the defining equation has the form of a function application. 
```{.haskell}
squares :: Integer -> Integer
squares num = num * num
```
If the argument in that application is a name (e.g. *num* in the
defining equation `squares num = num * num`), then when an application
is evaluated, the name will take on the argument value, and the
right-hand side of the function is evaluated. What happens when the
argument on the left-hand side is a constant? If the argument on the
left-hand side is a constant, then the right-hand side will be used only
if the function is applied to that value. In Haskell, a function can be
defined with several defining equations to have simple and more readable
code. For example, the definition of the function `inWords` has four
defining equations, each with a constant argument to return the English
spelling of the numeric value: 

```{.haskell}
inWords :: Integer -> String
inWords 0 = "zero"
inWords 1 = "one"
inWords 2 = "two"
inWords 3 = "three"
```
When the function application `inWords 1` is evaluated or when
`inWords` is called with `1`, the computer begins by checking the
defining equations (the patterns) from top to bottom. The first defining
equation will be checked, and discovers that the application `inWords 1`
doesn't match the left-hand side of the first line, `inWords 0`;
therefore the next equation is tried and it does match, so the
corresponding right-hand side (the function body) is then evaluated and
returned. An error message is printed, and the program will terminate if
`inWords` is applied to an argument like `7`, which does not match any
of the defining equations. To avoid such an error, include a *catchall*
pattern with a name that starts with a lowercase letter (like i, j,
xVal) as the last defining equation (pattern). The `inWords` function
can be redefined as in `inWords'`: 
```{.haskell}
inWords' :: Integer -> String
inWords' 0 = "zero"
inWords' 1 = "one"
inWords' 2 = "two"
inWords' 3 = "three"
inWords' i = "Not known to me"
```

Consider writing the same function with *if/then/else* rather than
pattern matching and it will be more complex and convoluted. Also if the
last pattern (`inWords' i = "Not known to me"`) is moved to the top, the
function would always print `"Not known to me"`, because the numbers
wouldn't have a chance to fall through and be checked for any other
patterns.

A function can be defined to test for a specific argument pattern. The
following function `is_four` will return `True` if the argument pattern
is a `4` and `False` otherwise. 

```{.haskell}
is_four :: Integer -> Bool
is_four 4 = True
is_four x = False
```
The same function can be defined using the wildcard pattern `_`, in
place of the variable `x`, because the variable is not used in the body
of the function. Also, the "don't care" pattern is better documentation
than the use of a variable name and might even be more efficient,
depending on optimisation settings. 
```{.haskell}
is_four' :: Integer -> Bool
is_four' 4 = True
is_four' _ = False
```

The function `is_four` has two defining equations, if the argument
pattern `4` in the first defining equation matches the argument, then
that equation is used, otherwise the second equation is checked; the
argument `x` or `_` will match *any Integer* argument, so the
corresponding value `False` is returned. For example, given the
application `is_four (2*2)`, the argument is evaluated to `4`; since
this matches `4`, the first defining equation is used and `True` is
returned. Similarly, given the application `is_four (5-2)`, the argument
is evaluated to `3`; since this does not match `4`, the second defining
equation is used.

A function may have more than one argument. If this is the case, then
each argument in a given defining equation is checked, from left to
right. For example, here is the `conjunction` function, which returns
`True` if and only if both of the arguments are `True`: 
```{.haskell}
conjunction :: Bool -> Bool -> Bool
conjunction    True    True  = True
conjunction    _       _     = False
```


Consider the evaluation of the application `conjunction True False`.
The first defining equation is checked from left to right at runtime.
The first argument of the first pattern matches, so the second argument
on that line is also checked. It does not match, so the second defining
equation is checked; its left-hand side matches both arguments, so the
corresponding value `False` is returned.

Pattern matching is commonly used when the argument to a function is a
tuple or any value built by applying a constructor to arguments. For
example, suppose we want a function `first` that takes a pair and
returns its first element, and a similar function `second` to return the
second element. These two functions can easily be defined using pattern
matching as follows:

```{.haskell}
first :: (i, j) -> i
first    (x, _) =  x

second :: (i, j) -> j
second    (_, y) =  y
```

When `first (9,2)` is evaluated, the argument value `(9,2)` is matched
against the argument pattern `(x, _)` in the defining equation; `x` is
bounded to `9` and `_` to `2`. Then the right-hand side `x` is returned,
giving the result `9`. The `first` and `second` functions are very
useful, so they are already defined in the `Prelude` as `fst` and `snd`
respectively.

Now consider writing a function that takes two vectors in 2D space and
adds them together. This vector addition requires adding the
corresponding `x` components separately and the `y` components too
separately. Without pattern matching the function can be defined as
follows: 
```{.haskell}
add2DVec :: (Double, Double) -> (Double, Double) -> (Double, Double)
add2DVec    a                   b                =  (fst a + fst b, snd a + snd b)
```
A much better version of the same function which makes it clear that
the parameters are tuples, and increases readability by giving names to
the tuple components right away, can be defined using pattern matching
as follows: 
```{.haskell}
add2DVec' :: (Double, Double) -> (Double, Double) -> (Double, Double)
add2DVec'    (x1, y1)            (x2, y2)         =  (x1 + x2, y1 + y2)
```

In both `add2DVec` and `add2DVec'` functions, there is only one
pattern (defining equation) and it is also a *catchall pattern*.

An argument pattern may also be a list. Since a list is either `[]` or
else it is constructed with the *cons* (list construction) `:` operator,
the patterns take the forms \[\] and x:xs. For example, the following
function determines whether a list is empty: 
```{.haskell}
isEmpty :: [a] -> Bool
isEmpty    []  =  True
isEmpty (x:xs) = False
```
An alternative to `isEmpty` can be defined as follows, to show how
wildcard pattern `_` can be used here as well: 
```{.haskell}
isEmpty' :: [a] -> Bool
isEmpty'    []  =  True
isEmpty'    _   =  False
```

The parentheses around the pattern `x:xs` in the left-hand side of
`isEmpty` are required to avoid the parse error - "error: Parse error in
pattern". The bracket-free default parse as `(isEmpty x):xs`, as
function application is always most tightly binding. Thus, to bind
something to several variables, surround them in parentheses so Haskell
can properly parse them. When `isEmpty []` is evaluated, the argument
matches the pattern in the first defining equation, so `True` is
returned. However, when the application `isEmpty (1:2:3:[])` is
evaluated, the argument fails to match `[]` and the second equation is
tried. Here the match succeeds, with `x` defined as `1` and `xs` defined
as `2:3:[]`, and `False` is returned. It makes no difference if the
argument is written with the simpler syntax `[1,2,3]`; this is merely an
abbreviation (or syntactic sugar) for `1:2:3:[]`, so the evaluation is
identical.

The following is an implementation of the head function using pattern
matching against lists: 
```{.haskell}
head' :: [a] -> a
head' [] = error "empty list!"
head' (x:_) = x
```
The `_` character means we don't care about that part and represents
an arbitrary value. As another example, this is a simple function that
takes a list and returns a list of characters or a String - naively it
prints its elements out in words. 
```{.haskell}
tellme :: (Show a) => [a] -> String
tellme [] = "The list is empty"
tellme (x:[]) = "The list has one element: " ++ show x
tellme (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tellme (x:y:_) = "The list has more than two elements. The first two elements are: 
 " ++ show x ++ " and " ++ show y
```
 Note that (x:\[\]) and (x:y:\[\]) could be rewritten as \[x\] and
\[x,y\].

There is a special type of pattern sometimes called an "at-pattern", but
"as-pattern" is preferred. As-patterns makes it possible to break up an
item according to a pattern, while keeping a reference to the entire
original item and so increasing efficiency : there is no need to rebuild
from its components a value that already exists. To create an
as-pattern, precede a regular pattern with a name and an `@` character.
For example the as-pattern `realL@(x:y:ys)` will match the same lists
that `x:y:ys` would and easily access the entire original list using
realL. Instead of needing to type out `x:y:ys`. The following is an
example of a simple function that uses an as-pattern: 
```{.haskell}
someLetters :: String -> String
someLetters "" = "Empty string"
someLetters realL@(x:[]) = "There is only one letter in " ++ realL ++ " which is " ++ [x]
someLetters realL@(x:y:xs) = "The first letter of " ++ realL ++ 
 " is " ++ [x] ++ " and the second is " ++ [y]
```


Lambda Expressions
------------------

Also known as anonymous functions, they are alternatives to defining
functions. Lambda expressions comprise of a pattern for each of the
arguments, a body that specifies how the results can be calculated in
terms of the arguments, but do not give a name for the function itself.
They can be used when a function is only needed once. To declare a
lambda expression, write a `\`, followed by the function's arguments
(which can include patterns) separated by spaces, then an arrow `->`,
and the function body. Lambda expressions are usually surrounded with
parentheses and can have only a single clause in its definition. This
limitation restricts how patterns can be used in the definition of a
lambda expression. The body could be a case expressions, for example: 
```{.haskell}
not True = False
not _    = True
```
compiles to `not = \ b -> case b of {True -> False; _ -> True}`. Here
`{...}` is equivalent to a new block, and ";" separates lines.

Redefining the `heads'` function as a lambda expression with one clause
that matches any pattern would fail when tested with an empty list and
described as `\(a:_) -> a`. This *heads expressions* can be used in a
function to double the head of a given list: 
```{.haskell}
doubleHeads :: [Integer]-> Integer 
doubleHeads    xs       =  2 * (\(x:_) -> x) xs
```
Note, when this function is called with a value on which pattern
matching fails (e.g. \[\]), an error will occur at runtime as the
definition *typechecks* and it will compile.

Just like functions, lambda expressions can take any number of
arguments: for example
`zipWith (\x y -> (x * y)/2) [4, 6, 2, 8] [3, 5, 7, 9]`. Lambda
expressions have several practical applications: 1. They can be used to
formalise the meaning of curried function definitions. 2. They are
useful when defining functions that return functions as results. 3. And
as already described, they can be used to avoid naming a function that
will be used only once.

Guards
------

Patterns operate on the values passed to the function and guards are
used to check properties of the values passed. Guards are very similar
to *if-then-else* conditional expressions but are more readable and used
in functions to check if some property of the passed values is true or
false.

Many times, the last guard in a function is *otherwise (= True in the
Prelude)*, which catches everything. If all the guards in a function
evaluate to `false`, and there isn't an otherwise *catchall guard*,
evaluation falls through to the next pattern: this is how patterns and
guards play nicely together. If no suitable guards or patterns are
found, an error is thrown. A function that returns the absolute value of
an integer can be defined using conditional expressions as: 
```{.haskell}
realAbs :: Integer -> Integer
realAbs a = if a < 0 then (-a) else (a)
```
*Note:* neither pair of brackets in the definition of `realAbs` is
necessary. In some circumstances the first pair is necessary to
disambiguate negation from subtraction. The same function can be defined
using guards as: 

```{.haskell}
realAbs' :: Integer -> Integer
realAbs' a
     | a < 0 = (-a)
     | otherwise = a
```

When guards are used in a function definition, each
guard begins with the *pipe* `|` and have its own "is defined" symbol
`=`, however, there is no `=` after the arguments of the function
definition. The pipe is followed by the guard case expression before the
`=` and must evaluate to either `true` or `false`. The `=` is also
followed by the expression which will be returned if the guard case
expression is `true`. The following function uses guards to determine if
a triangle is right angled or not. 
```{.haskell}
triAngle :: (Num x, Eq x) => x -> x -> x -> String
triAngle a b c
   | a^2 + b^2 == c^2 = "right-angled"
   | otherwise = "not right-angled" 
```

*Note* putting `=` after the function name and the arguments, before the
first guard will cause a syntax error.

where?!
-------

`where` is used in Haskell to perform the same task as you would do with
a variable in an imperative programming language. Thus, use `where` to
store intermediate results. `where` bindings are not shared across
function bodies of different patterns. `where` is very similar to `let`
expressions but since `let` has a value and `where` doesn't; `let` is
considered as an expression.

A function which takes total mark and number of modules, and returns the
grade category of a student can be defined using guards as follows: 
```{.haskell}
gradeCat :: Double  -> Double -> String
gradeCat tMark modules
  | tMark / modules >= 70.0 = "First"
  | tMark / modules >= 60.0 = "Upper"  
  | tMark / modules >= 50.0 = "Lower"
  | tMark / modules >= 45.0 = "Pass"  
  | otherwise = "Fail"  
```

*Note*, this is just an example and not the UK definition of grades!
The average mark `tMark / modules` is computed four times in the
`gradeCat`. This can be avoided by using the `where` keyword to bind the
value to a variable and use that variable in place of the average
calculations as follows: 
```{.haskell}
gradeCat' :: Double  -> Double -> String
gradeCat' tMark modules
  | avegM >= 70.0 = "First"
  | avegM >= 60.0 = "Upper"  
  | avegM >= 50.0 = "Lower"
  | avegM >= 45.0 = "Pass"  
  | otherwise  = "Fail"  
  where avegM = tMark / modules
```

This technique improves readability by giving names to things, and
may even make the programs faster, as the average mark is calculated
once. Variables defined in the `where` section of a function are only
visible to that function. Also, `where` bindings are not shared across
function bodies of different patterns. To further demonstrate how the
`where` can be used to make a program more readable, the same function
can be defined as follows: 
```{.haskell}
gradeCat'' :: Double  -> Double -> String
gradeCat'' tMark modules
  | avegM >= aFirst = "First"
  | avegM >= aUpper = "Upper"  
  | avegM >= aLower = "Lower"
  | avegM >= aPass = "Pass"  
  | otherwise  = "Fail"  
  where avegM = tMark / modules
        aFirst = 70.0
        aUpper = 60.0
        aLower = 50.0
        aPass = 45.0
```
Notice that all the variable names are aligned in a single column.
Haskell will get confused if they are not aligned, and it won't know
that they're all part of the same block.

Higher-Order Functions
----------------------

Higher-order functions allow common programming patterns to be
encapsulated as functions. In Haskell, functions are values and hence
can be passed around like any other values, making it possible to
combine functions efficiently. Functions in Haskell are *first class
objects*; that is, they can be stored in data structures, pass them as
arguments to functions, and create new ones. A function is called *first
order* if its arguments and results are ordinary data values, and it is
called *higher order* if it takes another function as an argument, or if
it returns a function as its result.

The `twice` function takes another function *g* as its first argument,
and it applies *g* two times to its second argument *y*:

```{.haskell}
twice :: (a->a) -> a -> a
twice g y = g (g y)
```
Looking at the `twice` function, if the second argument has type *a*,
then the argument function (*g*) must accept an argument of type *a* and
return a value of the same type. Thus, the argument function must have a
type *a-&gt;a*. In Haskell, functions with multiple arguments are
defined using the notion of *currying* and receive their arguments one
at a time. That is, if a function takes two arguments but applied to
just one argument, the evaluation gives a new function which is ready to
take the second argument to finish the computation. For example, the
following function takes two arguments and returns their sum:

```{.haskell}
mySum :: Integer -> Integer -> Integer
mySum a b = a + b
```

The function `mySum` can also be defined as
`mySum = \a -> (\b -> a + b)`, which states that `mySum` is a function
that takes an integer *a* and returns another function which also takes
an integer *b* and returns the sum *a + b*. A *full application* of
`mySum` is an expression giving `mySum` all its arguments: for example,
`mySum 7 9`. However, the *partial application* (or calling the function
with fewer arguments):`mySum 7` supplies just one argument, and the rest
of this is a new function that takes a number and adds it to 7.
Higher-order functions can be used to manipulate how functions are
applied to arguments. For example, partial application is a cleaver way
of creating functions on the fly by passing them to other functions.
Consider the following function: 

```{.haskell}
addthree :: Integer -> Integer -> Integer -> Integer
addthree a b c = a + b + c
```

When `addthree 2 3 4` or `((addthree 2) 3) 4`is called, `addthree` is
first applied to `2`; creating a function that takes one argument and
returns a function. Then that function is applied to `3`, which creates
a function that takes one argument, add `2` and `3` together, and then
add that to the argument. That function is applied to `4`, and the
results is `9`. The type declaration of `addthree` can also be written
as `addthree :: Integer -> (Integer -> (Integer -> Integer))`. The type
(or type variable) before the `->` is the type of the values that a
function takes, and the type after it is the type of the values it
returns. From the type declaration
`addthree :: Integer -> (Integer -> (Integer -> Integer))`, the function
takes a value `Integer` and returns a function of type
`(Integer -> (Integer -> Integer))`. Similarly, the returned function
takes a value of type `Integer` and returns a function of type
`Integer -> Integer`. And finally, the returned function takes a value
of type `Integer` and returns another value of type `Integer`.

