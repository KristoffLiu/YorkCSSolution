module Block2QA where  -- CHANGE TO MATCH FILENAME

{-
# SOF3/Block 2
-}
{-
# Problems
-}

{-
Problems in this set have been inspired by several sources, as well
from our own experience.  Two important sources are SOF1 and seminal
texts by [Henderson] [1] and by [Bird and Wadler][2].

These exercises are to ensure that you are able to use:
* the Haskell type system (a version of the Hindly-Milner
  type system extended with type classes).
* laziness in execution.

We will also now assume familiarity with the list comprehension
notation.  This is just a convenient syntax for combinations of `map`,
`filter` and `concat = foldr (++) []`.  It can be thought of as a
generalised nested for-loop structure.

## Study

You should read [Hutton][3] Chapters 3 (again!), 5, 8 & 15; ensure
that you do all the exercises.

You should study the two lectures "NewTypes" (again!) and "Laziness".

## Q1: List comprehension

### Q1.1

Rewrite the function `oneone` as the function `oneone'`, _without_
using list comprehension.
-}
oneone, oneone' :: [Int] -> [Int]
oneone ns = [ n+1 | n <- ns,  n `mod` 2 == 0 ]
oneone' = undefined

{-
### Q1.2

Rewrite the function `onetwo` as the function `onetwo'`, _without_
using list comprehension.
-}
onetwo, onetwo' :: [String] -> [Bool]
onetwo css = [(fromEnum c) `mod` 2 == 1 |
              cs <- css, length cs > 1, c <- cs, c `elem` ['a'..'z']]
onetwo' = undefined

{-
### Q1.3

Rewrite the function `onethree` as the function `onethree'`, _as_ a
list comprehension.

It uses  two utility  functions, `bitstring2int`, which  uses Horner's
rule, and  `evenparity`, a function pipeline  to compute the parity.
-}
bitstring2int :: [Bool] -> Int
bitstring2int = foldl (\ n b -> 2*n + if b then 1 else 0) 0

parity :: [Bool] -> Bool
parity = (==0) . (`mod` 2) . length . filter (== True) 

onethree, onethree' :: [[Bool]] -> [Int]
onethree = map bitstring2int . filter parity
onethree' = undefined


{-
## Q2: Maybe and Either types as replacements for exception handling

### Maybe types

Being a pure language Haskell does not have exceptions.  Instead the
return value from a function needs to indicate if it is a valid result
or not.  There are two types for doing this, `Maybe` and `Either`.
`Maybe` is the simpler solution, and `Either` the more sophisticated.
In this section we ask you to solve problems using the simpler `Maybe`.

The type `Maybe a` is defined:
```haskell
data Maybe a = Nothing | Just a
```
A satisfactory result is indicated by tagging it with `Just`, and an
unsatisfactory result by returning `Nothing`.  (This does not tell the
calling context what the error was; that is enabled by the more
sophisticated `Either` type.)

Consider the following function:
-}
ePbs2i :: [Bool] -> Int
ePbs2i bs | parity bs = bitstring2int bs 
{-
#### Q2.1

What happens if you evaluate `ePbs2i [True, False, True, False]` and
`ePbs2i [True, False, False, False]`?


#### Q2.2

Given a new version of the function that expects a `Maybe [Bool]` and
returns a `Maybe Int` instead.  It should return `Nothing` if its
input is `Nothing` or if the value has odd parity.
-}
ePbs2iM :: Maybe [Bool] -> Maybe Int
ePbs2iM = undefined
{-
Now suppose that we want to use the result of `ePbs2i'` as the input
to another function, such as:
-}
doubleOdd :: Int -> Int
doubleOdd n | odd n = n * 2
{-
Define a function `doubleOddM` that behaves like `doubleOdd`, except
that it expects and returns a `Maybe Int`.  Then we can form the
pipeline `doubleOddM . ePbs2iM`.
-}
doubleOddM :: Maybe Int -> Maybe Int
doubleOddM = undefined

{-
The solution looks a little ugly!  Later we will see that Haskell has
some useful facilities to help with these definitions.

One such facility is the _function_
```haskell
maybe :: b -> (a -> b) -> Maybe a -> b
```
Look up its definition, and use it to define the function `doepM`,
which either reports "Ooops!" if the input either represents an even
number or has odd parity, or the **string** representing the doubled
input (recall that the function `show` converts `Show`able values into
a string).

Your function should respect
```haskell
doepM [True, True]        == "6" -- even parity, representing an odd number
doepM [True, False]       == "Ooops!" -- odd parity
doepM [True, True, False] == "Ooops!" -- even number
```
-}
doepM :: [Bool] -> String
doepM = undefined

{-
### Either types

One of the problems with `Maybe` types is that if the final value
produced by the pipeline is `Nothing` we do not know what the error
was.  In the example above, we do not know if the problem was an odd
parity input or an even input, or both.

We can use `Either` types to improve this.
```haskell
data Either a b = Left a | Right b
```
By _convention_ the left type represents the error, and the right type
represents a correct value.

We can indicate errors by any suitable type: `String` and `Int` are
common, or a special purpose type.  Let us use `String`.  It is
convenient to introduce a synonym for this type.
-}

type Error a = Either String a

{-
Now we can update `ePbs2i`:
-}

ePbs2iE :: Error [Bool]          -> Error Int
ePbs2iE    (Left msg)             = Left msg
ePbs2iE    (Right bs) | parity bs = Right (bitstring2int bs)
                      | otherwise = Left "input has odd parity"

{-
#### Q2.3

Update `doubleOdd` to `doubleOddE`.  Hence update `doepM`, to `doepE`.
You may find the `Prelude` function `either` useful.  If the output
represents an error it should be preceded with the string "ERROR: ".
-}
doubleOddE :: Error Int -> Error Int
doepE :: [Bool] -> String
doubleOddE = undefined
doepE = undefined

{-
## Q3: Laziness

Every value constructor is **lazy** in every argument.  That means it
will not attempt to evaluate its argument unless its context demands
the value.  Even then, it only evaluates it enough to satisfy the
context.  This allows us to work with values of _unbounded_ size, as
long as its context only needs a _finite_ portion of it.

The name for an unevaluated expression is a **thunk**.  An expression
is represented at run-time by a graph with thunks as terminal nodes.
If the value of one of these thunks is demanded (hence this strategy
is sometimes called "call-by-demand") the thunk is minimally expanded
in to a new sub-graph.

### Q3.1

Reduce
```haskell
(if True then inc else dbl) (2+1)
```
by the normal order reduction strategy. (See the talk on "Laziness" in
Block 2.)
-}
{-
Laziness is often encountered with lists, but similar things can be
done with, for example, Binary trees.  We will work initially with the
recursively defined lists:
-}
ones, nats :: [Integer]
ones = 1 : ones
nats = 0 : map succ nats

{-
Note that when exploring values you should be careful to only examine
a finite portion of them.  (**Hint** consider the `Prelude` function,
`take`.)

Values can also be explored in the repl by using the `:sprint <variable>`
command, which displays the current structure associated
with the variable, printing unevaluated thunks as `_`.  (A more
detailed output can be obtained by using the command `:print` instead
of `:sprint`; `:sprint` is a _short_ version of `:print`.)

1. Reload the file, using `:r`, so that any execution history is reset.
2. Give the repl the command `:sprint nats`.
3. Evaluate the following expressions, and immediately after each
   evaluation reissue the command `:sprint nats`; this will show you
   what is currently expanded in memory.

```haskell
take 1 nats
take 6 nats
take 4 nats
```

### Q3.2

What is the base case in each of the recursive definitions of `ones`
and `nats`?

What is the recursive case?
-}

{-
### Q3.3 Recursions as fixed points

We can always write a recursive definition as a fixed point of some
function.  A fixed point of a function `f::a -> a` is a value `z::a`
such that:
```haskell
z = f z
```

A function can have none, one or many fixed points.  If it has many
fixed points, then we always get the **least fixed point**, that is
the one with the worst behaviour: this is the nature of computation.
Bad behaviour is non-termination (which may be an explicit error
message if we are lucky, or infinite internal computation if not.)

We can use the function `fix` to compute fixed points.  It is defined
in the library `Data.Function` but, rather than import the library, we
repeat the definition here:
-}
fix :: (a->a) -> a
fix    f       = f (fix f)
{-
Note that this concentrates all of the recursion into one place, the
`fix` function.  A definition `x = fix f` creates a graph with:
1. a subgraph for the thunk representing the function named `f` 
2. a root node, representing `x`, with a pointer to the Node at 3:
3. a node representing an application, with a pointer to the thunk
   representing `f` at Subgraph 1, and a pointer to the Node at 2.

Lazy evaluation is implemented by each time that there is a demand for
part of `x`, the application is unwound one step at a time until
enough of `x` has been computed to satisfy the demand.  In eager
evaluation, Node 3 is completely unwound (which may result in an
infinite computation) before considering any attempt to evaluate `x`.

For example, we could have defined `ones`:
-}
ones' :: [Integer]
ones' = fix (1:)
{-
Define a new version of `nats`, `nats'`, using `fix`. 
-}
nats' :: [Integer]
nats' = undefined

{-
#### Q3.4
Consider the Ackermann function, defined recursively:
-}
ackermann :: Integer -> Integer -> Integer
ackermann 0 n = n+1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))
{-
The importance of this function is that it is not _primitive
recursive_ (see THE3), and one of the first to be identified.
**Warning!** do not evaluate this function for first value greater
than `3`: it takes a very long time to evaluate (`ackermann 4 2` has
19,729 decimal digits).

Express `ackermann` using `fix`.
-}
ackermann' :: Integer -> Integer -> Integer
ackermann' = undefined

{-
### Q3.4

Redefine `bottom` using `fix`. (Again, see the talk on Laziness for
the original definition of `bottom`.)
-}
bottom :: a
bottom = undefined

{-
### Q3.5

Given as input a list that has two **consecutive** occurrences of the
same value, return that value.  If there several such values, return
the one nearest the start of the list.  You may assume that there is
at least one such

For example, the result when applied to `[9,8,7,6,5,4,3,2]++ones`
should be `1`.
-}
findPlateau :: Eq a => [a] -> a
findPlateau = undefined

{-
Look up the function `iterate`.  What does the function `mystery3_5`
compute?  **Hint** Consider `mystery3_5 tz 99`.
-}
tz :: Int -> Int
tz    n = negate (n `div` 2)

mystery3_5 :: Eq a => (a -> a) -> a -> a
mystery3_5 = (findPlateau .) . iterate

{-
### Q3.6

Write an expression to generate the list of **Mersenne numbers**,
positive integers, one less than a positive integer power of 2, in
order.
-}
mersenne :: [Int]
mersenne = undefined


{-
### Q3.7

A **Mersenne prime** is a Mersenne number that is also prime.  Using
your solution to `mersenne` and the following definition of primes
generated by the Seive of Eratosthenes, generate the list of Mersenne
Primes.

**Note** there is a function in `Data.List`, called `intersect` that
computes the intersection of a list and a **finite** list.  Hence it
will not work to compute the intersection in this case.  You will need
to generate the intersection by another method, taking advantage of
the fact that both infinite lists are ordered.
-}
eratosthenes :: [Int]
eratosthenes = sieve (map Just [2..])
  where
    sieve (Just n  : ns) = n : sieve (zipWith ($) mark_n ns)
      where mark_n = cycle (replicate (n-1) id ++ [const Nothing]) 
    sieve (Nothing : ns) = sieve ns
mersennePrime :: [Int]
mersennePrime = undefined
{-
**Warning!** When testing this, it is feasible to generate the first
five Mersenne Primes, but getting the sixth takes quite a while!

## Q4: Lazy trees

Consider the type definition:
-}

newtype Tree a = Tree [(a, Tree a)] deriving Show

{-
It describes a tree that can have many branches at each stage.  We
have **not** derived equality because it is not clear what it means
for two trees to be equal: does order of branches matter?  Is a tree
with a repeated subtree the same as one without repetition?  It will
depend on what we are modelling with trees.

We can use such trees to encode the possible behaviours of a system.
For example, suppose we have a type of actions, or **events**,
available to a vending machine:
-}
data VM_Event = Coin | Choc | Fudj | Halt deriving (Eq, Show)
{-
`Coin` represents the insertion of a £1 coin into a vending machine;
`Choc` represents the delivery of a bar of chocolate; `Fudj` the
delivery of a bar of fudge; and `Halt` represents the pressing of the
stop button.  So the list `[Coin, Choc]` represents a single
transaction.

The owner of a vending machine may want the property "Pay before
eating", so would be happy to turn the machine on and see `[Coin,
Choc]` but very unhappy to see `[Choc]`.  We can encode all possible
sequences of behaviours of a vending machine as a value of type
`Tree VM_Event`.  As these behaviours can be arbitrarily long, we will
need to take advantage of laziness. (This example is due to
[C. A. R. Hoare][5], who considers far more complex behaviours of
vending machines than we consider here).

The behaviour that combines "Pay before eating" with "maximum credit
£1", and then halts after two transactions is:
-}

type VM = Tree VM_Event

vm1_2 :: VM
vm1_2  = Tree [(Coin, Tree [(Choc, Tree [(Coin, Tree [(Choc, Tree [])])])])]

{-
A version that is eternal is:
-}
vm1_e :: VM
vm1_e = Tree [(Coin, Tree [(Choc, vm1_e)])]

{-
A tree that represents the behaviour of a dead system is:
-}
stop :: Tree a
stop = Tree []

{-
An operator that extends behaviour, **at the start**, is:
-}
leadsto :: a -> Tree a -> Tree a
e `leadsto` b = Tree [(e, b)]
{-
An operator that allows branching is:
-}
branch :: Tree a -> Tree a -> Tree a
Tree ts `branch` Tree us = Tree (ts ++ us)
{-
We can redefine `vm1_2` using these operators by:
-}
vm1_2' :: VM
vm1_2' = Coin `leadsto` (Choc `leadsto` (Coin `leadsto` (Choc `leadsto` stop)))
{-
### Q4.1

Redefine `vm1_e` using the new operators.
-}
vm1_e' :: VM
vm1_e' = undefined

{-
The trees `vm1_2` and `vm1_e` do not branch.

A potentially eternal version, with a `Halt` button, and which has two
branches at each level, and using our new operators, is:
-}
vm1_h :: VM
vm1_h = (Coin `leadsto` ((Choc `leadsto` vm1_h)
                         `branch`
                         (Halt `leadsto` stop)))
        `branch`
        (Halt `leadsto` stop)

{-
### Q4.2

A machine with behaviour `vm1_h` can be halted at any time,
potentially losing a customer their payment.  Define a nicer vending
machine behaviour that can only be halted between transactions.  You
may either do it directly, or using the operators.
-}
vm1_h' :: VM
vm1_h' = undefined

{-
### Q4.3

We cannot display a potentially infinite behaviour.  However, we can
display the behaviour up to some given depth: this is a generalisation
of `take::Int->[a]->[a]` to `Tree`.

Write a function to cut off a tree at a given depth, so that the
result can be displayed.
-}
takeTree :: Int -> Tree a -> Tree a
depth = undefined

{-
### Q4.4
Write a function to count the number of nodes in a finite tree.
-}
countTree :: Tree a -> Int
countTree = undefined

{-
### Q4.5

Write a function that replaces labels in a tree by the result of
applying a function to the label.  This is a generalisation of `map`
to `Tree`.

For example, replacing in `vm1_2` according to the function:
-}
choc2fudj :: VM_Event -> VM_Event
choc2fudj    Choc      = Fudj
choc2fudj    x         = x
{-
gives
```haskell
Tree [(Coin, Tree [(Fudj, Tree [(Coin, Tree [(Fudj, Tree [])])])])]
```
and replacing according to the function `length . show` gives
```haskell
Tree [(4, Tree [(4, Tree [(4, Tree [(4, Tree [])])])])]
```
-}
mapTree :: (a -> b) -> Tree a   -> Tree b
mapTree = undefined

{-
Being able to map over a structure is a very common problem, so much
so that Haskell has a type class for asserting that a function is a
map over a type.  The type class is called `Functor`, with the
function `fmap`.  We will study this in a later block.  For now we can
just assert that:
-}

instance Functor Tree where
  fmap = mapTree

{-
which effectively renames `mapTree` to the standard name for such a
function, `fmap`.  Indeed, `map` is retained for lists purely for
historical reasons.

## Q5 A longer example

Look at the "Snakes and Ladders" example.  Use this as a template to
build a similar library for "Noughts-and-Crosses", also known as
"Tic-Tac-Toe".  You will need to deal with the problem that a play
(the equivalent of a dice roll in "Snakes and Ladders") may be illegal
in the current state; in this case you should just ignore the play.
-}

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

[5]: C. A. R. ("Tony") Hoare, _Communicating Sequential Processes_,
     Prentice Hall, 1985.  [Electronic
     version](http://www.usingcsp.com/)
-}
