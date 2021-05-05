module Strict where
import Data.Foldable (foldl')
import Control.DeepSeq

{-
# Strictness
# Jeremy Jacob 29 March 2021

## Reading
[Hutton][1], Section 15.7 (you may wish to re-read the rest of Chapter
15 to set the context.)

## Introduction

We started with a _pure_ (side-effect-free), lazily-evaluated language
of expressions that were built by applying functions to arguments.  We
then re-introduced a controlled degree of _impurity_ using _monads_.
Now we introduce **relaxing laziness**.

Recall that:
* an evaluation scheme applied to an expression will either terminate
  with answer or behave like ⊥ (run forever, or crash);
* all evaluation schemes that terminate for a given expression give
  the same answer;
* if any evaluation scheme terminates for an expression then the lazy
  evaluation scheme terminates for it; and
* the lazy evaluation scheme evaluates only needed parts of
  expressions, and evaluates them only once.

In addition lazy evaluation allows us to modularise our algorithms,
cleanly separate **control** from **data**, for example:
-}
firstNineSquaresOfEvenInts :: [Int]
firstNineSquaresOfEvenInts = take 9 $ map (^2) $ filter even $ [0..]
{-
Here the original data is the _infinite_ list `[0..]`, and it is
processed by a pair of functions, `filter even` and `map (^2)`, both
of which produce infinite lists.  The control statement is `take 9`.

So why would we want to (selectively) abandon laziness?

The answer is to do with efficiency: laziness requires that the
implementation keep subexpressions that _might_ need to be evaluated.
(Once the implementation is sure that a subexpression is not needed it
can be marked for later _garbage collection_.)  If the value of a
subexpression will definitely be needed then it is usually better to
evaluate it eagerly.

Deciding if an expression is always needed or not is achieved through
**strictness** analysis.  A function is **strict** if:
```haskell
f ⊥ == ⊥
```
An example of strict and non-strict functions are:
-}
strict, nonstrict :: a -> Int
strict    = const undefined
nonstrict = const 0
{-
Less contrived examples of strict and nonstrict functions are `foldl`
and `foldr`, which we repeat here specialised to lists.
```haskell
foldl :: (b -> a -> b) -> b -> [a]   -> b -- strict in 3rd argument
foldl    _                z    []     = z
foldl    f                z    (x:xs) = foldl f (f x z) xs
foldr :: (a -> b -> b) -> b -> [a]   -> b -- nonstrict in 3rd argument
foldr    _                z    []     = z
foldr    f                z    (x:xs) = f x (foldr f z xs)
```
The key issue here is that evaluation of `foldl` cannot give any
result until all of its input has been consumed (the case of
application to `[]`), while `foldr` can, depending on `f`, give a
partial result having only consumed a finite portion of its input.

Recall that `id::[a]->[a]` can be expressed (inefficiently) as `foldr
(:) []`. For example, if we want the first element of the result of applying `foldr (:) []` to an infinite list lazy evaluation proceeds:
```haskell
head (foldr (:) [] [0..])
~~> -- [0..] == (0:[1..])
head (foldr (:) [] (0:[1..]))
~~> -- foldr
head (0 : foldr (:) [] [1..]))
~~> -- head
0
```
For some functions `foldr` may need to consume the whole list.  A good
example is `foldr (+) 0`: `(+)` needs the value of both arguments before
it can produce any output.

However, `foldl` **must always** consume the whole list before it can
give a result.  Hence if the list is infinite, evaluation will not
terminate.  A typical example is the linear algorithm to reverse a
list, `foldl (flip (:)) []`, and suppose that we only want the first
element of the result:
```haskell
head (foldl (flip (:)) [] [0..])
~~>
head (foldl (flip (:)) (flip (:) [] 0) [1..])
~~>
head (foldl (flip (:)) (flip (:) (flip (:) [] 0) 1) [2..])
~~>
...
```
This clearly never reaches the base case `foldl (:) zs []`,
and nor is any partial result available before termination.

Hence we may as well evaluate `foldl` eagerly, and avoid the overheads
associated with lazy evaluation.

The module `Data.Foldable` contains a function `foldl'` which is an
eager version of `foldl`.  **In practice** you should always prefer
`foldl'` to `foldl`.  Hence the best way to define `reverse` is:
-}
reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []
{-
## Adding strictness

The whole issue is complex: there is the danger of turning a
terminating expression into a non-terminating expressions, and adding
uneccessary computation.  To quote [Hutton, p226][1]:
> Even for relatively simple examples, the use of strict application
> is a specialist topic that requires careful consideration of the
> behaviour of lazy evaluation.

There are two basic primitives: `seq :: a -> b -> b` (in `Prelude`)
and `deepseq :: a -> b -> b` (in `Control.DeepSeq`).  These are both a
little like `flip const`, _but_ `seq` guarantees to evaluate its first
argument to _weak head normal form_ (evaluation halts as soon as the
outermost constructer is determined), and `deepseq` guarantees to
evaluate its first argument to _normal form_ (evaluation is halted
only when all constructors are detemined), whereas `flip const` never
evaluates its first argument.  Here is an illustration:
-}
type MC = Maybe Char
flconst :: b -> a -> a
flconst = flip const
ex0, ex0', ex0'', ex1, ex1', ex1'', ex2, ex2', ex2'' :: ()
ex0   = let x = undefined      in (x :: MC) `flconst` () -- result
ex0'  = let x = Just undefined in (x :: MC) `flconst` () -- result
ex0'' = let x = Nothing        in (x :: MC) `flconst` () -- result
ex1   = let x = undefined      in (x :: MC) `seq`     () -- fail
ex1'  = let x = Just undefined in (x :: MC) `seq`     () -- result
ex1'' = let x = Nothing        in (x :: MC) `seq`     () -- result
ex2   = let x = undefined      in (x :: MC) `deepseq` () -- fail
ex2'  = let x = Just undefined in (x :: MC) `deepseq` () -- fail
ex2'' = let x = Nothing        in (x :: MC) `deepseq` () -- result
{-
(The explicit type annotations are to tell the compiler that `x` is
appropriate as the first argument to `deepseq`.  I have placed them in
the other expressions for uniformity.)

The value `undefined` can never be evaluated to weak head normal form,
let alone normal form; `Just undefined` is in weak head normal form,
but can never be evaluated to normal form, and `Nothing` is in normal
form, and hence also in weak head normal form.

All of `ex0`, `ex0'`, and `ex0''` return a value, because `flconst`
never inspects its first argument.  The expression `ex1` fails,
because `undefined` does not have a weak head normal form; however,
`Just undefined` and `Nothing` are already in weak head normal form,
so `ex1'` and `ex1''` return results.  Finally, neither of `ex2` and
`ex2'` return results, because in neither case can `x` be evaluated to
normal form; however `ex2''`, where `x` is in normal form, does return
a result.

The most common way of adding strictness to an expression is to use
the operator `($!)`.  This is a replacement for `($)`, and uses `seq`
to force the argument into weak head normal form:
```haskell
f $! x = x `seq` f x
```
In the module `Control.DeepSeq` is an operator, `($!!)`, based instead
on `deepseq`.

Here is how we could define a function to sum a necessarily finite
list of `Int`s:
-}
sum' :: [Int] -> Int
sum' = sum_acc 0
  where
    sum_acc n []     = n
    sum_acc n (x:xs) = (sum_acc $! (n + x)) xs
{-
Here the first parameter of `sum_acc` accumulates the answer, with
each addition happening as soon as possible, as the algorithm runs.
If the `($!)` operator were replaced with ordinary function
application, the accumulator would be a series of _thunks_, or
potential evaluations, that could only be evaluated once the input
list was fully consumed.  So without strict evaluation more space
would be used, and a little more time to construct the series of
thunks and then evaluate them, each one involving an addition.

Note that the evaluation of the list parameter to `sum'` is still
lazy.

Of course, it would be better software engineering to not define
`sum'` from first principles, but to use `foldl'` from
[`Data.List`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html) or [`Data.Foldable`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Foldable.html):
-}
sum'' :: [Int] -> Int
sum'' = foldl' (+) 0
{-
This gives the identical algorithm.

## Strict types

It is possible to tell Haskell to make some types strict by making
their constructors strict.  For example, a strict version of lists is:
-}
infixr 2 :!
data StrictList a = StrictEmpty | a :! !(StrictList a)
  deriving (Eq, Show)
instance Functor StrictList where
  fmap f = ff
    where
      ff StrictEmpty = StrictEmpty
      ff (x :! xs)   = f x :! ff xs
{-
Here the annotation `!` before the second parameter to `StrictCons`
declares the parameter to be strict: that is it will always be
evaluated when the program attempts to build a value using `(:!)`.
The head of the list is evaluated lazily: there is no `!` annotation.
This version of lists is called _spine-strict_, as the structure of
the list is fully evaluated, but its elements may still be unevaluated
thunks.

Consider the examples:
-}
type StrictString = StrictList Char
ex4, ex5 :: StrictString
ex4 = 'a' :! 'b' :! 'c' :! 'd' :! StrictEmpty
ex5 = succ <$> ex4
{-
If we examine these in the repl, using `:print ex4` and `:print ex5`
we see that neither are evaluated:
```haskell
λ> :print ex4
ex4 = (_t1::StrictString)
λ> :print ex5
ex5 = (_t2::StrictString)
```
We can evaluate these to weak head normal form, by _one_ evaluation
each of `seq ex4 0` and `seq ex5 0`, and then examine them we get:
```haskell
λ> ex4 `seq` ()
()
λ> ex5 `seq` ()
()
λ> :print ex4
ex4 = :! 'a' (:! 'b' (:! 'c' (:! 'd' (_t3::StrictList Char))))
λ> :print ex5
ex5 = :!
        (_t4::Char)
        (:!
           (_t5::Char)
           (:! (_t6::Char) (:! (_t7::Char) (_t8::StrictList Char))))
```
Note that the spine is fully evaluated in both cases, but for `ex5`
the expressions denoting each element have yet to be evaluated.  In
`ex4` the elements are already in normal form.  In neither case is the
`StrictEmpty` evaluated.  To further evalute these structure we need
to write functions that fully traverse them.  The function `print` is
such a function, so `show ex4` and `show ex5` completely force the
evaluations (recall that `print` is implicitely called at the outemost
level of each expression submitted to the repl):
```haskell
λ> ex4
'a' :! ('b' :! ('c' :! ('d' :! StrictEmpty)))
λ> ex5
'b' :! ('c' :! ('d' :! ('e' :! StrictEmpty)))
```

Care is needed, as any attempt to build an infinite `StrictList` will
result in infinite behaviour.

Also note that we cannot apply `deepseq` to values of type
`StrictList` without venturing into parts of Haskell beyond those
covered in this module.

## Reference
[1]: Graham Hutton, _Programming in Haskell_, 2nd edition, 2016,
     Cambridge University Press.
-}
