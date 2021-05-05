module Block1QA where -- CHANGE TO MATCH FILENAME

{-
# SOF3/Block 1
# Guide to solutions

Problems in this set have been inspired by several sources, as well
from our own experience.  Two important sources are SOF1 and seminal
texts by [Henderson] [1] and by [Bird and Wadler][2].

These exercises are to ensure that you are:
* able to run Haskell
* can exploit the facilities of `GHCI`, the Haskell repl
  (Read-Evaluate-Print Loop);[Hoogλe][4] may also be helpful.
* can create simple definition files, and load them into the repl, and
* most importantly, become familiar with evalauable expressions using
  the definitions in the `Prelude`.

## Study

You should read [Hutton][3] Chapters 1–⁠4, 6, 7; ensure that you do all
the exercises.

You should study the two lectures "WhyFP" and "NewTypes".

## Q1: Evaluating simple expressions

Start up `ghci`.  (How you do this will depend on your chosen
operating system and access method.  One of us prefers to run `ghci`
through `emacs` on *nix-based OSs, such as MacOS and Linux.  Another
route is to give the command `ghci` to a shell running in a terminal.)

For each of the following expressions
* describe the expression in words,
* give its value,
* try it in the repl, and
* explain any differences to yourself.

1. `3 + 8`
2. `3^2`
3. `2^3^4 == (2^3)^4`
4. `2^3^4 == 2^(3^4)`
5. `0==1 || 0/=1`
6. `'h' : "ello"`
7. `7 : [6, 5, 4]`
8. `if 0==1 then "Alice" else "Bob"`
9. `let sqr = \ n -> n^2 in sqr 3 + sqr 4`
10. `let cube n = n^3 in cube 2 + cube 3`

## Q2: Determining types

The repl will also tell you the type of an expression.  The command
has the form: `:type <expression>` ("`type`" can be abbreviated to
`"t"`, so the command becomes `:t <expression>`).

Commands to the repl take the general form `:<name> <parameters>`;
`<name>` can be abbreviated to the shortest string that is different
from any lexicographically earlier command.  The full list is
available through `:help`.  The command `quit` will terminate the
repl.

For each of the expressions above:
* decide what type it has,
* try it in the repl, and
* explain any difference to yourself.

## Q3: Detecting errors

A useful question to ask about any language is not what it allows, but
what **errors** it forbids.  There are various classes of error,
including:
* Syntactic (parse) errors
* Static semantic errors (includes typing errors — as in the type of
  the value, not keyboard skills!)
* Dynamic semantic errors

All except dynamic semantic errors are detectable at compile time by
automated analysis.


Each of the following expressions contains at least one error.  For
each:
* determine what kind of error it is,
* check with the repl (if possible),
* consider possible fixes, and
* decide if the error would be detected in other languages you know.

(One of the expressions is arguably not an error: which?  why might it
be controversial?)

1. `if 0 then 'a' else 'b'`
2. `if 'a' == 'b' then 5`
3. `if 'a' = 'b' then 5 else 4`
4. `if 0 == 0 then 5 else 'b'`
5. `['0', 5, 9]`
6. `('a', 42`
7. `let sumSq m n = m^2 - n^2 in sumSq 3 4`
8. `let f (x:xs) = 42 in f []`
9. `let f n = if n == 0 then 0 else f (n+1) in f 3`
10. `* 2 3`
11. `"3333" :: Int`

## Q4: Simple expressions that use values defined in `Prelude`

For each of the following names defined in the `Prelude` use
[Hoogλe][4] to find a description of their value.  Where names are
related, they are grouped.

1. `fst`, `snd`
2. `curry`, `uncurry`
3. `succ`, `pred`
4. `negate`, `abs`, `signum`
5. `quot`, `rem`, `div`, `mod`, `divMod`
6. `gcd`
7. `id`, `const`, `(.)`
8. `flip`, `($)`
9. `error`, `undefined`
10. `map`, `(++)`, `filter`
11. `head`, `tail`, `last`, `init`
12. `(!!)`, `null`
13. `length`, `reverse`
14. `foldr`, `foldr1`, `foldl`, `foldl1`
14. `scanl`, `iterate`, `repeat`, `replicate`, `cycle`
15. `take`, `drop`, `takeWhile`, `dropWhile`, `break`, `splitAt`
16. `zip`, `zipWith`, `unzip`
17. `lines`, `words`, `unlines`, `unwords`

These are only a small proportion of the useful functions defined in
`Prelude`.

For each of the following expressions:
* describe the expression in words,
* give its type and value,
* try it in the repl, and
* explain any differences to yourself.

1. `length (drop 3 "hello world!")`
2. `take (length "hello") "goodbye all!"`
3. `dropWhile (/=' ') "hello world!"`
4. `filter (/=' ') "hello world!"`
5. `foldr (+) 0 [3, 5, 7]`
6. `foldl (+) 0 [3, 5, 7]`
7. `(foldr (-) 0 [3, 5, 7], foldl (-) 0 [3, 5, 7])`
8. `foldr (*) 1 [3, 5, 7]`
9. `zip [0..] ['A'..'Z']`
10. `takeWhile ((<9).fst) $ zip [0..] ['A'..'Z']`
11. `zip [0..] (take 9 ['A'..'Z'])`
12. `if 0==0 then "OK" else undefined`
13. `if 0==1 then "OK" else undefined`

## Q5: Simple functions

In this section you are asked to devlop some simple functions.  The
type of each is given, and a prototype implementation.  Holes in the
definition are marked by the special value `undefined`.  These will
compile, but any attempt to execute `undefined` results in termination
of the execution.

Each function is described in words and test examples given

### Q5.1

Construct a string containing `n` so that `greet "Bob" == "hello Bob!"`

The operator `(++)` joins lists together, strings are lists of characters
-}

greet :: String -> String
greet name = "Hello " ++ name ++ "!"

greetTest :: Bool
greetTest
  = greet "Kofi" == "Hello Kofi!"
    && greet "Jeremy" == "Hello Jeremy!"
    && greet "" == "Hello !"

{-
### Q5.2

Calculate the cost of a number of cakes (See SOF1)
```haskell
cakeBill 2 3 == "The cost of 2 cakes at 3p each is 6p."
```
`show` converts values that have a string representation to a string
Example: `show 42 == "42"`.
-}

cakeBill :: Int -> Int -> String
cakeBill quantity price =
  "The cost of " ++ show quantity ++ " cakes at " ++ show price
  ++ "p each is " ++ show (quantity * price) ++ "p."

cakeBillTest :: Bool
cakeBillTest =
  cakeBill 0 3 == "The cost of 0 cakes at 3p each is 0p."
  && cakeBill 1 3 == "The cost of 1 cakes at 3p each is 3p."
  && cakeBill 2 3 == "The cost of 2 cakes at 3p each is 6p."

{-
The function `cakeBill` does not get the case of 1 cake quite right.
Give a function `cakeBill'` that uses correct English grammar.
-}

cakeBill' :: Int -> Int -> String
cakeBill' quantity price =
  "The cost of " ++ show quantity ++ " cake"
  ++ (if quantity==1 then "" else "s") ++ " at " ++ show price
  ++ "p each is " ++ show (quantity * price) ++ "p."

cakeBill'Test :: Bool
cakeBill'Test =
  cakeBill' 0 3 == "The cost of 0 cakes at 3p each is 0p."
  && cakeBill' 1 3 == "The cost of 1 cake at 3p each is 3p."
  && cakeBill' 2 3 == "The cost of 2 cakes at 3p each is 6p."

{-
### Q5.3

See SOF1, Week 2, Exercise 3

A fruit company sells bananas for £3.00 a kilogram plus £4.99 per
order for postage and packaging. If an order, including postage and
packing, is over £50.00, the P&P is reduced by £1.50. Write a function
that will take the weight of bananas as input and print the cost of
that order.

Extra to SOF1, Week 2, Exercise 3, the function should raise an error
if less than 2kg of bananas is ordered.
Assume that orders must be whole numbers of kilos, and cost is
expressed in in pennies.
-}

bananas :: Int -> Int
bananas order
  | order < min_order      = error "Too small an order!"
  | raw_cost < pandp_limit = raw_cost
  | otherwise              = raw_cost - pandp_reduction
  where
    raw_cost = costperkg * order + pandp
    min_order = 2
    costperkg = 300
    pandp = 499
    pandp_limit = 5000
    pandp_reduction = 150

bananasTest :: Bool
bananasTest =
  bananas 2 == 1099
  && bananas 20 == 6349

{-
### Q5.4

Showing the cost of an order in pennies is not pretty.  Write a
function to pretty print a number of pennies as pounds and pence.
Function `divMod` from `Prelude` may be useful.

Recall that a pattern can be used to get the parts of a pair.
For example:
```haskell
(x, y) = (2+1, 4*2)
```
defines`x` to be `3` and `y` to be `8`.

**NOTES**
* From now on, we only provide the minimal skeleton to allow the file
to pass the syntax and typing checks.  You will need to decide on
numbers and names of parameters, whether guards are needed, and so on.
* From now on you will need to construct your own tests.

-}

pennies2pounds :: Int -> String
pennies2pounds pennies = show pounds ++"." ++ show pence
  where
    (pounds, pence) = pennies `divMod` 100

{-
### Q5.5

The implication operator on Booleans is not defined in `Prelude`.

Define it using other operators defined in `Prelude`.  Can you solve
the problem without explicit parameters?
-}
implies :: Bool -> Bool -> Bool -- explicit parameters
implies a b = (not a) || b -- explicit parameters
implies_ :: Bool -> Bool -> Bool -- implicit parameters
implies_ = (||) . not -- implicit parameters

{-
It is easy to define Boolean operators using truth tables.
For example we could define `(&&)` by:
```haskell
(&&) :: Bool -> Bool -> Bool
True  && True  = True
True  && False = False
False && True  = False
False && False = False
```

An alternative is to use the "don't care" pattern, `_`, and take
advantage of the fact that patterns are matched in order:

```haskell
True && True = True
_    && _    = False
```

Define the implication operator twice more, once using a full truth
table and once using "don't care" patterns.
-}

implies', implies'' :: Bool -> Bool -> Bool
implies' True True   = True
implies' True False  = False
implies' False True  = True
implies' False False = True

implies'' True False = False
implies'' _     _    = True

{-
### Q5.6

There is a well-known puzzle about a farmer taking a dog, a chicken
and a bag of grain across a river on a boat that will only take the
farmer and two of the items.  If the dog is left alone with the
chicken, it will eat the chicken, and similarly if the chicken is left
alone with the grain. (Other variants of the items exist, such as
wolf, goat and cabbage.)

We can model the goods by a new type, `Item` that has equality &
inequality defined on it, and can be converted to a string for
printing:
-}

data Item = Dog | Chicken | Grain deriving (Eq, Show)

{-
Create a function `eats` that returns the list of `Item`s eaten by the
input item.
-}

eats :: Item -> [Item]
eats Dog     = [Chicken]
eats Chicken = [Grain]
eats Grain   = []

{-
Create a function `danger` that, given two `Item`s, reports if either
will eat the other.  You should use the function `eats` you have just
defined.  You may find the `Prelude` function `elem` useful.
-}

danger :: Item -> Item -> Bool
danger x y = x `elem` eats y || y `elem` eats x

{-
## Q6: Recursive functions

### Q6.1

Write an explicitely recursive function, `incList` that returns a
result where each element is one more than the corrsponding input.
For example,
```haskell
incList [42, 7, 16] == [43, 8, 17]
```
Do this _without_ using any functions from `Prelude`.
-}

incList :: [Int] -> [Int]
incList []     = []
incList (n:ns) = n+1 : incList ns

{-
Haskell has lots of functions in its libraries that capture particular
patterns of recursion.  The pattern in `incList` is _mapping_, found
in `Prelude`:
```haskell
map f [a, b, c] == [f a, f b, f c]
```
Define `incList'` using `map`.
-}
incList' :: [Int] -> [Int]
incList' = map (+1)

{-
### Q6.2

The testing functions above are not very convenient, as they report
`False` if _any_ conjunct evaluates to `False`: it will not tell us
which conjuncts evaluated to `False`.  We can do better by using `map`
to apply a testing function to a range of inputs.

As an example, rewrite `greetTest` as `greetTest'`.

**Hint** map a function over a list of pairs, where each element has
the form `(input, expectedOutput)`.  The function can conveniently be
defined in a `where` clause.
-}
greetTest' :: [Bool]
greetTest'
  = map gt [("Kofi", "Hello Kofi!"),
            ("Jeremy", "Hello Jeremy!"),
            ("", "Hello !")]
  where
    gt (input, expectedOutput) = greet input == expectedOutput

{-
### Q6.3

Suppose that you are given a list that is known to contain a
particular element, but not its index.  Create a function `pos` to
return the index; if there is more than one, the smallest index should
be returned.  Indices start from `0`.  For example,
```haskell
pos 'h' "hello world" == 0
pos 'e' "hello world" == 1
pos 'l' "hello world" == 2
```
You may assume that the type in the list has _type class_ `Eq`.  That
is similar to saying that the type in the list implements the `Eq`
interface in an object oriented language.  It is indicated by the
prefix `Eq a =>` in the type, which we can read "Assuming `a` has type
class `Eq`...".

**Hint** define, and then use, a recursive, subsidiary function that
has type `Int -> [a] -> Int`; but note that if you use a `where`
clause to define the subsidiary function you will not be able to enter
the type in your file.
-}

pos :: Eq a => a -> [a] -> Int
pos x = posx 0
  where
    posx n (y:ys) | x==y      = n
                  | otherwise = posx (n+1) ys

{-
### Q6.4

Define a function that inserts its value in the correct place in an
ordered list.  You may assume that the standard equality (`(==)`,
`(/=)`) and ordering operators (`(<)`, `(<=)`, `(>)`, `(>=)`) are
defined for the type of elements under consideration.
-}
insert :: Ord a => a -> [a] -> [a]
insert x = insx
  where
    insx [] = [x]
    insx ys@(z:zs) | x <= z    = x : ys
                   | otherwise = z : insx zs
{-
Another important pattern of recursion is right-folding.

Recall that a list such as `[5, 7, 6, 2]` is actually syntactic sugar
for `5:(7:(6:(2:[])))`.  A right fold replaces the list-constructor,
"cons", `(:)` by an operator and the empty list, `[]`, by a constant.

For example, the right fold with `(+)` and `0` computes the sum of the list:
`5+(7+(6+(2+0)))`; the right fold with `(*)` and `1` computes the product:
`5*(7*(6*(2*1)))`.

It is called a right-fold because the expression associates to the
right.  The function is called `foldr`, and can be defined:
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op unit []     = []
foldr op unit (x:xs) = x `op` (foldr op unit xs)
```
(The definition in `Prelude` is an optimised version.)

### Q6.5

Note that the operator need not be homogenous (`a -> a -> a`) but can
be more general (`a -> b -> b`).  Take advantage of this fact to
define insertion sort using `foldr` [hint: consider `insert` above].
For example:
```haskell
isort [9,8,7,6] == [6,7,8,9]
isort "hello" == "ehllo"
isort [True, False, True] == [False, True, True]
```
-}
isort :: Ord a => [a] -> [a]
isort = foldr insert []

{-
### Q6.6

It is possible to define insertion using a right fold, so that `isort`
is a nested pair of right folds.  Redefine insertion as a right fold.
-}
insert' :: Ord a => a -> [a] -> [a]
insert' x = foldr insx [x]
  where
    insx y ys@(z:zs) | z==x && x<y = z:y:zs
                     | otherwise   = y:ys

{-
### Q6.7

What does the following function, `mystery`, do?  (Think about it
before you try executing it.)
-}
mystery :: [a] -> [a]
mystery = foldr (:) []
{-
This function is the identity on lists: it replaces `(:)` by `(:)` and
`[]` by `[]`!.

The function `id` is far more efficient!
-}

{-
### Q6.8

The map pattern is a special case of a right fold.  Define `mapAsRF`
to behave like `map`, using `foldr`.
-}

mapAsRF :: (a -> b) -> [a] -> [b]
mapAsRF f = foldr ((:) . f) []

{-
### Q6.9

Sometimes a right fold is less efficient than a left fold.  The left
fold of `(+)` and `0` over `[5, 7, 6, 2]` is `(((0+5)+7)+6)+2`.  When
a homogenous (that is, has type `a -> a -> a`) operator is
_associative_, such as `(+)`, `(*)`, and `(++)`, the left and right
folds are equal.  When the operator is not associative, such as `(-)`,
the two folds may give different answers.  When the operator is not
homogenous (that is, it has type `a -> b -> b` for _different_ `a` and
`b`), the left fold of `flip f` may be different to the right fold of
`f`.

Evaluate `foldr (+) 0 [5, 7, 6, 2]` and `foldl (+) 0 [5, 7, 6, 2]` by
hand.  Be careful to preserve the bracketing of expressions. [You will
need to look up the definition of `foldl`; when evaluating `foldl`
always evaluate any additions that occur as soon as a possible (this
is actually `foldl'` rather than `foldl`, which does not matter for
this exercise).]
-}

{-
```haskell
foldr (+) 0 [5, 7, 6, 2]
~~>
foldr (+) 0 (5:(6:(7:(2:[]))))
~~>
5 + foldr (+) 0 (6:(7:(2:[])))
~~>
5 + (6 + foldr (+) 0 (7:(2:[])))
~~>
5 + (6 + (7 + foldr (+) 0 (2:[])))
~~>
5 + (6 + (7 + (2 + foldr (+) 0 [])))
~~>
5 + (6 + (7 + (2 + 0)))
~~>
5 + (6 + (7 + 2))
~~>
5 + (6 + 9)
~~>
5 + 15
~~>
20

foldl (+) 0 [5, 7, 6, 2]
~~>
foldl (+) 0 (5:(6:(7:(2:[]))))
~~>
foldl (+) (0+5) (6:(7:(2:[])))
~~>
foldl (+) 5 (6:(7:(2:[])))
~~>
foldl (+) (5+6) (7:(2:[]))
~~>
foldl (+) 11 (7:(2:[]))
~~>
foldl (+) (11+7) (2:[])
~~>
foldl (+) 18 (2:[])
~~>
foldl (+) (18+2) []
~~>
foldl (+) 20 []
~~>
20
```

Both evaluations take the same number of steps, but the left fold
takes much less space to evaluate than the right fold, which must
build up a chain of unevaluated additions until the end of the list is
reached.
-}


{-
A left fold can be much less efficient than a right fold.  When
evaluated on an infinite list it never gives any result, whereas a
right fold can, in some circumstances, produce partial results.

### Q6.10

Express reverse of a list twice, once as a right fold and once as a
left fold.
-}
revRF, revLF :: [a] -> [a]
revRF = foldr (\ x xs -> xs ++ [x]) []
revLF = foldl (flip (:))            []
{-
The functions `revLF` and `mystery` show that `foldl (flip f)` and
`foldr f` may give different results.
-}

{-
### Q6.11
Express `length` of a list as both a right and a left fold.
-}
lenRF, lenLF :: [a] -> Int
lenRF = foldr (const (1+))        0
lenLF = foldl (flip (const (1+))) 0

{-
### Q6.12

(See SOF1, Week 7.)

We can define a new type to represent vectors of Integers:
-}

type Vector = [Int]

{-
Define scalar multiplication of vectors.  We will use the infix symbol
`(/*/)` to represent this operator.

```haskell
x /*/ [a, b, c] == [x * a, x * b, x * c]
```
-}

(/*/) :: Int -> Vector -> Vector
(/*/) = map . (*)

{-
We can also define addition of vectors.  We will use the symbol `(/+/)`.
```haskell
[a, b, c] /+/ [d, e, f] == [a+d, b+e, c+f]
```

The problem here is that we have two lists, so `map` only does the job
if we convert the pair of lists into a list of pairs.  This is best
done with the `Prelude` function
`zipWith::(a->b->c) -> [a] -> [b] -> [c]`
that generalises `map` to pairs of lists.
```haskell
zipWith op [a, b, c] [d, e, f, g] == [a `op` d, b `op` e, c `op` f]
```
(Note that the output is the length of the shortest input.)
-}
(/+/) :: Vector -> Vector -> Vector
(/+/) = zipWith (+)

{-
We can define a unit vector for `(/+/)` by
-}
zeroV :: Vector
zeroV = repeat 0
{-
Define a function that sums a list of vectors.
-}
sumV :: [Vector] -> Vector
sumV = foldr (/+/) zeroV

{-
In SOF1/Practical 6A/Exercise 6 you are asked to implement equality on
vectors.

You do not need to implement equality, or inequality, on the Haskell
type `Vector`: it is predfined in `Prelude` for all list types whose
contents have equality defined, and any renamings, such as `Vector`.
-}


{-
### Q6.13

Define your own version of `zipWith` using explicit recursion.
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f = zwf
  where
    zwf _  [] = []
    zwf [] _  = []
    zwf (x:xs) (y:ys) = f x y : zwf xs ys

{-
### Q6.14

See SOF1/Practical 7/Exercise 4

Implement a function `merge :: Ord a => [a] -> [a] -[a]`, that
expectes two _ordered_ lists of any type that can be compared, and
merges the two lists into an ordered list.
-}
merge :: Ord a => [a] -> [a] -> [a]
merge xs         []         = xs
merge []         ys         = ys
merge xs@(x:xs') ys@(y:ys') | x < y     = x : merge xs' ys
                            | otherwise = y : merge xs  ys'

{-
Define a test `isOrdered :: Ord a => [a] -> Bool` that returns `True`
exactly when the parameter is ordered.

**Hint** Use zipWith on the parameter and its tail to get a `[Bool]`.
The list can then be combined with the `Prelude` function
`and::[Bool] -> Bool]`, defined by:
```haskell
and = foldr (&&) True
```
-}
isOrdered :: Ord a => [a] -> Bool
isOrdered xs = and (zipWith (<=) xs (tail xs))
{-
### Q6.15

See SOF1/Practical 7/Exercise 5

Write a function `something_ish :: Eq a => [a] -> [a] -> Bool` and
hence a function `elfish`.

**Hint** Use the predefined right-fold/map combinators
`all :: (a->Bool) -> [a] -> Bool)` and
`elem :: Eq a => a -> [a] -> Bool` 
-}
something_ish :: Eq a => [a] -> [a] -> Bool
elfish :: String -> Bool
something_ish pat target = all (`elem` target) pat
elfish = something_ish "elf"

{-
## References

[1]: Peter Henderson, _Functional Programming: Application and
     implementation_, Prentice Hall, 1980.

[2]: Richard Bird and Philip Wadler, _Introduction to Functional
     Programming_, Prentice Hall, 1st edition, 1988.

[3]: Graham Hutton, _Programming in Haskell_, Cambridge, 2nd edition,
     2016.  JMB shelfmark: SK 59 HAS/H; [Electronic
     version](https://doi-org.libproxy.york.ac.uk/10.1017/CBO9781316784099)

[4]: <https://hoogle.haskell.org/> Neil Mitchell, _Hoogλe_
-}
