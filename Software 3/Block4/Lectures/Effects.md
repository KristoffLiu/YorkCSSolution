```haskell
module Effects where
import System.IO -- for file IO

```
# Effects through type class `Monad`
# Jeremy Jacob 1 Jan 2021

## Introduction

As we have noted before, _ad_ _hoc_ effects cannot coexist with lazy
evaluation.  However a certain sub-class of effects, **monadic**
effects, can co-exist with lazy evaluation.  These are achieved
through the type class, `Monad`.  `Monad` builds on another type
class, `Applicative`, which itself builds on the type class `Functor`;
this is rather like `Monoid` building on `Semigroup`.  We will look at
all three of these.

The concept of _monad_ is beginning to find its way into other
languages because they have uses beyond adding effects to laziness;
for example, _Java_, _Python_, _C++_, _C#_, _F#_ (where they are
called "computation builders"), and _Scala_.  This parallels the
gradual acceptance of anonymous functions (λ-expressions) from
functional programming languages into mainstream imperative languages.

Monads, like monoids, are another concept from abstract algebra.
Monads are a concept that arose within the broader concept of
_category theory_; but you don't need to know anything about this to
use them!  As monoids are an abstract way to study associative binary
operators with a neutral element, monads are an abstract way to study
systems that behave like the λ-calculus, and this gives a clue as to
why they might co-exist with laziness.

The insight to use monads for effects comes from [Philip Wadler](http://homepages.inf.ed.ac.uk/wadler/).
Later retellings of of his original lecture on the utility of monads
in computation are available on YouTube; for example, [one given in
Australia in 2013](https://youtu.be/yjmKMhJOJos).

We will now work our way through the type classes, starting with
`Functor` and ending with `Monad`.

## The type class `Functor`

We are used to the function
```haskell
map :: (a -> b) -> [a]   -> [b]
map    _           []     = []
map    f           (x:xs) = f x : map f xs
```
that lifts functions in a pointwise way from the underlying types (`a`
and `b`) to the world of lists over those types (`[a]` and `[b]`).
But many structures can have functions lifted pointwise, not just lists.

For example, we might want to lift functions to `Maybe` types:
```haskell
mapM :: (a -> b) -> Maybe a -> Maybe b
mapM    _           Nothing  = Nothing
mapM    f           (Just x) = Just (f x)
```
This is exactly what [the `Functor t` type class](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor) is for:
announcing that the type constructor `t` has an associated lifting
function.  There is one important method, `fmap`:
```haskell
fmap :: (a -> b) -> t a -> t b
```
For lists, `fmap = map`, and for `Maybe`, `fmap = mapM`.

`Prelude` provides the alias `(<$>)` for `fmap`, so instead of
`fmap f xs` we may write `f <$> xs`.  Compare this with `f $ x`, for
unlifted application.

There are many instances of `Functor` provided in the base libraries,
including lists and `Maybe`: [see the documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor).
We can think, for example, of the functorial aspect of `Maybe` as
allowing computations that might fail, propagating failure.  The
functorial aspect of lists is, essentially, SIMD concurrency.
Normally these are thought of as completely different things, but now
we can see that they are the same thing, parameterised by different
type constructors.

Functors are also a concept from category theory, and actually
comprise the pair of type constructor and `fmap`.  They ought to obey
the laws:
* **Identity** `fmap id == id`
* **Distributivity** `fmap (f . g) == fmap f . fmap g`

which assert that a functor preserves the _monoidal_ structure of
function composition.

## The type class `Applicative`

Consider the functorial aspect of `Maybe`: it allows us to apply a
function to data that might represent "error", preserving the "error".
But what if the function being applied is also capable of being
undefined?  The obvious thing to do is to "apply" a `Maybe (a->b)` to
a `Maybe a` to get a `Maybe b`.  A value of `Maybe (a -> b)` is not a
function, and so cannot be applied to anything, without further work.
The type class [`Applicative t`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Applicative), for type constructor `t`, handles
exactly this.

The methods include:
```haskell
pure :: a -> t a
(<*>) :: t (a -> b) -> t a -> t b
```
which form a minimal definition set.  More, derived, methods are
listed in [the documentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Applicative).

The function `pure` embeds a scalar value in the structure, so
`pure 0 ~~> [0]` in lists, and `pure 0 ~~> Just 0` in maybes.

The operator `(<*>)` is what solves the problem of allowing a
structure of functions to be "applied" to a structure of scalars.
For example,
```haskell
ex1, ex2, ex3 :: Num a => Maybe a
ex1 = Just (+1) <*> Just 0 -- Just 1
ex2 = Nothing <*> Just 0 -- Nothing
ex3 = Just (+1) <*> Nothing -- Nothing
ex4, ex5, ex6 :: Num a => [a]
ex4 = [(+1), (*2)] <*> [3, 4, 5] -- [4, 5, 6, 6, 8, 10]
ex5 = [] <*> [3, 4, 5] -- []
ex6 = [(+1), (*2)] <*> [] -- []
```
Note how every function in a list is applied to every value in the
argument.  This models non-deterministic computation, where both the
function and its argument are subject to non-determinism.  The empty
list, `[]`, gives us another way of modelling error or impossibility.

We can apply sequences of functions embedded in structures to a value
embedded in structures, just as we can apply a sequence of functions
to a value.  For example:
```haskell
ex7, ex8, ex9 :: Num a => Maybe a
ex7 = pure (+1) <*> (pure (*2) <*> (pure (+3) <*> Just 0)) -- Just 7
ex8 = pure (+1) <*> (pure (*2) <*> (pure (+3) <*> Nothing)) -- Nothing
ex9 = Nothing <*> (pure (*2) <*> (pure (+3) <*> Just 0)) -- Nothing
ex10, ex10a, ex10b :: Num a => [a]
ex10 = pure (+1) <*> (pure (*2) <*> (pure (+3) <*> [0, 1, 2])) -- [7, 9, 11]
ex10a = pure (+1) <*> (pure (*2) <*> (pure (+3) <*> [])) -- []
ex10b = [] <*> (pure (*2) <*> (pure (+3) <*> [0, 1, 2])) -- []
```

Again, there are many instances of `Applicative` in the base libraries.

There are four laws, that an applicative ought to obey, to ensure
structural integrity:
* **Identity** `(pure id <*>) == id`
* **Composition** `(pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))`
* **Homomorphism** `(pure f <*> pure x) == (pure (f x))`
* **Interchange** `(u <*> pure y) == (pure ($ y) <*> u)`

_Identity_ is about ensuring that an applicative version of identity
behaves like an identity.

_Composition_ looks complicated, but is essentially about applicative
composition, `pure (.)`, having similar properties to plain
composition, `(.)`.

_Homomorphism_ asserts the motivating property, for the special case
of `pure` values.

_Interchange_ says that applying the function embedded in `u` to the
scalar `y` is the same as applying the function `($ y)` (apply to `y`)
to the function embedded in `u`.

## The type class `Monad`

We have seen that viewing the applicative aspect of `Maybe` allows us
to build sequences of "functions" that preserve error indications.
But functions in the sequence cannot introduce error.  What if we
wanted to introduce a function such as `pure (div 4)` into the
sequence, and it received a value of `pure 0`?  There are various _ad
hoc_ solutions, but monads provide a consistent solution.

The important methods in type class `Monad t` are
```haskell
(>>=) :: t a -> (a -> t b) -> t b
return :: a -> t a
```
* `return` is just an alias for `pure`, kept for historical reasons.
* "bind", `(>>=)`, is a version of _application_ that allows a monadic
  value to have a scalar-to-monadic function applied to it.  Oddly,
  the scalar appears on the left and the function on the right.  There
  is an operator that takes its parameters in the standard order,
  `(=<<) = flip (>>=)`, and where more convenient we will discuss
  that. `(=<<)` is the operator that lets us apply a scaler-to-monadic
  function to a monadic value.

A good way to think about monads is that they embed part of the
computation in the _application operator_, bind (`>>=`), rather than
it all being in the function.  So `(=<<)` is a "smart" version of
ordinary application, `($)`.  See further below.

The monad operators must satisfy:
* **Left identity** `return a >>= k == k a`
* **Right identity** `m >>= return == m`
* **Associativity** `m >>= (\x -> k x >>= h) == (m >>= k) >>= h`

At first sight, monads are clumsy to use.  For example, we end up
writing expressions such as
```haskell
ex11, ex12, ex13, ex13a :: Num a => Maybe a
ex11 = (\x->return (x+1)) =<< pure 0
```
rather than
```haskell
ex12 = pure (+1) <*> pure 0
```
(Note the above two examples, and the next two, are ambiguous unless
we force their type.  Try changing `Maybe Int` to `[Int]`, and to
`Either String Int` and replaying examples `ex11`–`ex13a`. This shows
how different effects can be achieved by moving programs between
monads.)

The syntactic clumsiness is handled by a syntactic sugar for monads:
the `do` notation.
```haskell
ex13 = do
         x <- pure 0
         return (x+1)
```
It is deliberate that this looks like code in an imperative
programming language.  First `x` is bound to the value contained in
the monadic value produced by the expression `pure 0`, then the
function returns the value `x+1`.

In `ex13` we had to embed `0` in the monad, and then immediately
extract it.  In suxh a case we can use a different syntax:
```haskell
ex13a = do
          let x = 0
          return (x+1)
```
The `let` construct is a version of Haskell's usual `let` notation, so
care is needed.  The expression:
```haskell
do
  let x = 0 -- Line (1)
  let x = x+1 -- Line (2)
  return x
```
is an infinite computation, as Line (2) is a recursive definition of
`x`, rather than a definition in terms of the immediately previous
definition of `x` on Line (1).

We can rewrite `ex10` using `do` notation:
```haskell
ex14 :: Num a => [a]
ex14 = do
         x <- [0, 1, 2] -- [0, 1, 2] is a value in the monad
         x <- pure (x+3) -- x+3 is not a value in the monad, but pure(x+3) is.
         x <- pure (x*2) -- x*2 is not a value in the monad, but pure(x*2) is.
         return (x+1) -- x+1 is not a value in the monad, but return(x+1) is.
```
While this looks like repeated assignment to a variable `x`, with an
unusual syntax for the assignment statement; it is not.  Each `x` on
the right hand side is a _new_ variable that hides previous instances
of `x` in later lines.  It translates into the bound variable of a
λ-abstraction when de-sugared.  Also note the effect of the list
monad: `x` on the first line is bound to each value in the list in
turn, and, the result of applying the functions `(+3)` and `(*2)` is
placed in the output list.

A version using `let` is:
```haskell
ex14a :: Num a => [a]
ex14a = do
          x <- [0, 1, 2] -- [0, 1, 2] is a value in the monad
          let x' = x+3 -- neither x' nor x+3 are values in the monad
          let x'' = x'*2 -- neither x'' nor x'+2 are values in the monad
          return (x''+1) -- x''+1 is not a value in the monad, unlike return(x''+1)
```
Now let's consider functions that can introduce error notifications,
such as we might require on division by zero. We can write:
```haskell
ex15 :: Int -> Maybe Int
ex15    n    = do
                 x <- pure (negate n)
                 x <- pure (x + 3)
                 x <- if x==0 then Nothing else pure (4 `div` x)
                 return (x+1)
```
although it would be better to define:
```haskell
safediv :: Int -> Int -> Maybe Int
safediv    _      0    = Nothing
safediv    m      n    = Just (m `div` n)
```
and write:
```haskell
ex16 :: Int -> Maybe Int
ex16    n    = do
                 x <- pure (negate n)
                 x <- pure (x + 3)
                 x <- 4 `safediv` x
                 return (x+1)
```
Note how this includes "pure" expressions, that cannot introduce error
notifications, and "impure" expressions that may introduce error
notifications.

Here is a further example, using the list monad.  It is often an early
example in logic programming courses.  Given some people, and a
"parent" relationship, coded as a function from a person to a list of
parents, it shows how to define functions to compute lists of
grandparents and lists of ancestors.
```haskell
data Person = Alice | Bob   | Charlie | Dave
            | Eve   | Frank | Gina    | Helen
              deriving (Eq, Ord, Show)
parents, grandparents :: Person -> [Person]
parents Alice = [Bob, Eve] -- the parents of Alice are Bob & Eve
parents Bob   = [Frank, Helen]
parents Eve   = [Charlie, Gina]
parents Dave  = [Helen] -- only one parent is recorded for Dave
parents _     = [] -- parents of others not recorded

grandparents p =
  do x <- parents p -- for each parent
     parents x -- return their parents

ancestors p =
  do x <- parents p -- for each parent
     return x ++ ancestors x -- return parent, plus parent's ancestors

```
A typical Prolog solution looks something like:
```prolog
parent (Alice, Bob).
parent (Alice, Eve).
parent (Bob, Frank).
parent (Bob, Helen).
parent (Eve, Charlie).
parent (Eve, Gina).

grandparent (p, q) :- parent(p, x), parent(x, q).
ancestor (p, q) :- parent(p, q).
ancestor (p, q) :- parent(p, x), ancestor(x, q).
```

## Summary of `application` operators

For some type constructor `t`, the types of the four "application"
operators are:
```haskell
($)   :: (a -> b)   ->   a ->   b
(<$>) :: (a -> b)   -> t a -> t b
(<*>) :: t (a -> b) -> t a -> t b
(=<<) :: (a -> t b) -> t a -> t b
```
Each gives us different, useful, effects when arranging computations.
Riccardo Odone has [a nice 6-minute YouTube video](https://youtu.be/YS7DWacP55U) explaining this in
more detail.

## How monads help with patterns of computation

Recall _Problems for Block 2_, Q2, where we looked at pipelines that
propagated error notifications.  Both `Maybe` and `Either` types were
used, with `Either` types giving an increased sophistication in error
messages.  Here we will use only the simpler mechanism of `Maybe`
types, and leave you to explore `Either` types.

You were asked to define functions:
```haskell
bitstring2int :: [Bool] -> Int
bitstring2int = foldl (\ n b -> 2*n + if b then 1 else 0) 0

parity :: [Bool] -> Bool
parity = (==0) . (`mod` 2) . length . filter (== True)

ePbs2iM :: Maybe [Bool] -> Maybe Int
ePbs2iM Nothing   = Nothing
ePbs2iM (Just bs) | parity bs = Just (bitstring2int bs)
                  | otherwise = Nothing

doubleOddM :: Maybe Int -> Maybe Int
doubleOddM Nothing  = Nothing
doubleOddM (Just n) | odd n     = Just (n*2)
                    | otherwise = Nothing
```
Both of these functions accept a value that may signal an error
(`Nothing`), or maybe a good value (`Just x`).

The programmer has to take care of propagating errors, as seen above
in `ePbs2iM` and `doubleOdd`.  Further, when building a pipeline, the
programmer has to ensure that the input to the pipeline is correctly
tagged:
```haskell
doepM :: [Bool] -> String
doepM = maybe "Ooops!" show . doubleOddM . ePbs2iM . Just
```
The `Maybe` monad takes care of propagating errors for us.  The
definition of _bind_ for `Maybe is:
```haskell
(Just x) >>= k      = k x
Nothing  >>= _      = Nothing
```
If the input signals error, then the _bind_ operator is responsible
for propagating the notification, and not the function.

Note also that the function, `k`, accepts a scalar, so if we build our
pipeline with such functions then we do not need that initial
conversion using `Just`.

So now we can define:
```haskell
ePbs2iMD :: [Bool] -> Maybe Int
ePbs2iMD bs | parity bs = pure (bitstring2int bs)
            | otherwise = Nothing

doubleOddMD :: Int -> Maybe Int
doubleOddMD n | odd n     = pure (n*2)
              | otherwise = Nothing
```
The only place where the `Maybe` type leaks into these definitions is
in the expression returned when the guard fails.  We have even been
able to remove the explicit `Just` in the working case, by using the
`Applicative` method `pure`.

Now we can write a pipeline that returns a monadic value as:
```haskell
doepMD :: [Bool] -> Maybe Int
doepMD bs = ePbs2iMD bs >>= doubleOddMD
```
or, using the `do` notation:
```haskell
doepMD' :: [Bool] -> Maybe Int
doepMD' bs = do
               n <- ePbs2iMD bs
               doubleOddMD n
```
If we want to extract the final value from the monad we can, for
example, use the `maybe` function as before:
```haskell
doep :: [Bool] -> String
doep = maybe "Oooops!" show . doepMD
```
If we want the actual integer, and **not** the monadic value, then we
might write something such as:
```haskell
doep', doep'' :: [Bool] -> Int
doep' = maybe defaultValue id . doepMD
  where
    defaultValue = 0
doep'' = maybe (error "Input invalid") id . doepMD
```
depending on whether there is a sensible default value or not.

We can move a value from the `Maybe` monad to the `Either` monad, for
more sophisticated future error notification; however any errors
signalled in the `Maybe` monad cannot be recovered:
```haskell
doep''' :: [Bool] -> Either String Int
doep''' = maybe (Left "Unknown error") Right . doepMD
```
## How monads solve the problem of assignment combined with laziness

Consider a standard imperative language, and suppose we wrote (using
`:=` as the _ad hoc_ assignment operator):
```
function yyy() {
  x := 0;
  y := x;
  x := 1;
  print y}
```
What is the value that will be printed?  In an ordinary, eager,
language, it has to be `0`.  In a lazy language:
1. Execution starts at the end, with the call to `print y`.
2. The call forces the value of `y` to be evaluated, which forces the
   assignment `y := x` to be evaluated.
3. This in turn forces the value of `x` to be evaluated; but which
   value of `x`?  The latest update that might have had an effect on
   the value of `x` is the assignment `x := 1`, and this does not have
   any further dependencies, so `y` ends up referring to the value
   `1`, which is printed.

In practice it can be very hard in a lazy language to work out when a
value is assigned to a variable, and so combining lazy evaluation and
effects often causes surprises that are hard to fix.

In the equivalent monadic program,
```haskell
ex17 :: IO ()
ex17 = do
          x <- pure 0 -- (1)
          y <- pure x -- (2)
          x <- pure 1 -- (3)
          print y
```
the value printed is guaranteed to be `0`.  The `x`s on Lines (1) and
(2) are the same variable: Line (1) declares the new variable `x`, and
Line (2) references it; however, the `x` on Line (3) is a new
declaration in a new scope and is a different variable to that
declared on Line (1).  Scopes start with a declaration just to the
left of a left-pointing arrow and extend downwards to the next
declaration of the same name, or the end of the function.

## The `IO` monad and mixing monads

Let's reconsider the example `ex16`.  Suppose we want to print the
calculated value.  We can read from and write to the outside world
using [the `IO` monad](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:IO).

It is important to note that `IO` is a **type constructor**.  For
example, the type `IO Int` has values that _return_ an `Int` in the
normal way, but which also have (side) _effects_ of reading and writing
(lists of) `Char`acters to and from the outside world.  Reading is
from the standard input stream, usually attached to the keyboard, and
writing to the standard output stream, usually attached to the
terminal in which the program is running.

At the point when our programs are outputting to the outside world we
are not usually interested in the returned value.  Hence the standard
output methods of the `IO` monad have a type, `IO ()`, based on the
unit type, `()`, whose only value is also written `()`.  These
include:
```haskell
putChar :: Char -> IO () -- write one character to the standard output stream
putStr :: String -> IO () -- write a string to the standard output stream
putStrLn :: String -> IO () -- write a string and a newline character to the standard output stream
print :: Show a => a -> IO () -- write a Show-able value to the standard output stream
```
For reading we have:
```haskell
getChar :: IO Char -- read one character from the standard input stream, and return it
getLine :: IO String -- read a string from the standard input stream, and return it
interact :: (String -> String) -> IO () -- see below
```
Given a function `f :: String -> String`, `interact f :: IO ()` is a
value which reads a string from the standard input stream, applies `f`
to the string, and then writes the result to the standard output
stream.

Note that there are no functions to input, for example, an `Int`.  We
need to convert an input string to an `Int`.  This is most easily done
using the method `read` of the `Read` type class.  However, `read s`
will crash if `s` does not represent an `Int`, or whatever type `read`
is required to return by its context.  So we need to use an idiom,
such as
```haskell
readInt :: IO Int
readInt = do
            putStrLn "Enter a list of digits"
            s <- getLine
            if all (flip elem ['0' .. '9']) s
              then
                return (read s)
              else
                do
                  putStrLn "Input is not a list of digits"
                  readInt

```

> **Exercise** The function `readInt` is lacking in that it does
> **not** handle a leading negation sign, nor an empty input.  Fix
> this.
>
> Note that by importing `Data.Char`, we can replace `(flip elem ['0'
> .. '9'])` by the neater `isDigit`.

There is a more complex collection of values in the `IO` monad for
writing to and reading from other channels, such as files.

Now that we understand the `IO` monad, we can attempt to write a
version of `ex16` that has the effect of outputting the result as well
as returning it.  The following function is rejected by the compiler.
```haskell
ex16b n = do
            x <- pure (negate n)
            x <- pure (x + 3)
            x <- 4 `safediv` x -- Line (3)
            x <- x+1 -- Line (4)
            putStrLn ("The result of ((-'n'+3)/4)+1 is: " ++ show x)
            return x
```
Part of the problem is that `putStrLn` ought to be a value in
`Maybe Int`, but it is a value in `IO ()`.  Another part of the problem is,
what if Line (3) returns `Nothing`?

Here is one solution.  Its key feature is that the monadic computation
is separated, with an explicit transfer of values between the monads.
Computation in the `Maybe` monad is confined to `ex15`, separating it
from the computation in the `IO` monad through the main function body.
Again, we use the `Prelude` function `maybe` to test the result of the
calculation, and transfer the result between monads.  To record the
result as a plain value, rather than a value inside the monad, we use
the `let` notation, in the version used inside `do` blocks.
```haskell
ex16c :: Int -> IO Int
ex16c n = do
            let (msg, val) = maybe ("Division by zero", n)
                                   (\v->("Result is: " ++ show v, v))
                                   (ex16 n)
            putStr msg
            putStrLn (", on input: " ++ show n)
            return val
```
Note that in the error case we must return _something_; we choose to
return the input value, but we could have chosen, for example, `0` or
`1`; an even better choice would be to return a `Maybe Int`, as
follows:
```haskell
ex16d :: Int -> IO (Maybe Int)
ex16d n = do
            let result = ex16 n
            let msg = maybe "Division by zero"
                            (\v->"Result is: " ++ show v)
                            result
            putStr msg
            putStrLn (", on input: " ++ show n)
            return result
```
A further improvement uses the `Either` type constructor, rather than
the `Maybe` type constructor.  We leave this as an exercise: `ex16`
will need to be modified.

In summary: we can combine lazy evaluation (and type-safety!) with
effects, as long as we restrict ourselves to monadic effects.

## More general IO, with files

Haskell has facilities for general IO, reading and writing files.  The
facilities for reading and writing to the stanadrd input and output
streams are special cases of the general facilities.  While reading
and writing from the standard streams is available through `Prelude`
the more general facilities are only available by `import`ing the
module `System.IO`.

A problem with all general purpose IO systems is that each operating
system has its own way of dealing with peripheral devices.  The usual
fix is to have a language view and an operating system view, and as
little code as possible to connect the two.  In the case of Haskell,
the language deals with values of type `Handle`.  Each such value is
an alias for an input or output port, including files.  Two important
handles are:
* `stdin`, a handle for the standard input stream, and
* `stdout`, a handle for the standard output stream

From now on we will only consider files and the standard streams.

`System.IO` provides a type called `FilePath`, which is just an alias
for `String`.  Suitable values of `FilePath` are operating system
dependent.  For Unix-based systems, such as Linux and MacOS, files in
the current directory of the executing program can be referred to
simply by their name, without a full path.

When a handle is created for a file it is given an `IOMode`, one of:
* `ReadMode`, for reading only
* `WriteMode`, for _overwriting_ the file
* `AppendMode`, for adding to the end of the file
* `ReadWriteMode`, for more complicated interactions

To open a file a file there is a function
```haskell
openFile :: FilePath -> IOMode -> IO Handle
```
and to close it a function
```haskell
hClose :: Handle -> IO ()
```

As using these function comes with the danger of getting the protocol
wrong there are much more convenient functions that correctly call
`openFile` and `hClose`:
```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
readFile :: FilePath -> IO String
writeFile, appendFile :: FilePath -> String -> IO ()
```
The last three are available through `Prelude`.

Here are two examples that work together:
```haskell

greetfile :: FilePath
greetfile = "greetings.txt"

recordGreeting :: IO ()
recordGreeting = do
  putStrLn "What is your name? (terminated by a carriage return)"
  name <- getLine
  withFile greetfile WriteMode
    (\hdl -> do
        let hp = hPutStrLn hdl
        hp ("Hello " ++ name ++"!")
        hp "Welcome to Haskell file IO")
  putStrLn ("A greeting has been written to the file \""
             ++ greetfile
             ++ "\"")
  putStrLn "Bye!"

whoIsHere :: IO ()
whoIsHere = do
  s <- readFile greetfile
  putStr "The person greeted is: "
  putStrLn (takeWhile (/='!') (drop 6 s))
```
## Creating your own, special purpose monads

We discuss this topic as an extended example, for self-study, on
generating fresh register names during the translation phase of
compilation, in a separate file.
```haskell


