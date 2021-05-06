module Translate where
import NAbsSyn

{-
# Fresh identifier generation using monads: extended example
# Jeremy Jacob 5 Jan 2021

## The problem

There are many situations when our programs need to generate an
unknown number of new names.  These include the translation phase of a
compiler, where an abstract syntax term from one (usually high-level)
source language is converted to an abstract syntax term in another
(usually low-level) target language.  Often, when the translation
needs a variable in the target language a new name needs to be
invented (sometimes a name used in the source program can be reused).

An obvious solution in a lazy language is to have an infinite list of
new names, such as:
-}
name :: Show s => s -> String
name = ('$':) . show

newnames :: [String]
newnames = name <$> [0..]
{-
assuming that names of the form "$1234" are legal in the target
language, and not in the source language.

The problem is then for the translator to pick the next element of the
list.  We will illustrate this with a tiny fragment of a machine language.
-}
newtype M = M [Instr] deriving (Eq, Show)
data Instr =
  -- below:
  -- pc is the register representing the program counter
  -- l* is the address (line number) of label l
  -- Miscellaneous
    Label String -- pc := pc+1 (labels to be the target of a jump)
  -- Arithmetic
  | MovI String Int -- MovI r n = r := n; pc := pc+1
  -- Program counter manipulation
  | Halt         -- pc := Nothing
  | JumpL String -- pc := l* (jump to label)
  deriving (Eq, Show)
{-
We will also use two examples of N code:
-}
expgm :: Expr -> N
expgm g = N (Ifte g ("var" := ValInt 1) ("var" := ValInt 0))
-- if g then var := 1 else var := 0 end
ifTru, ifFls :: N
ifTru = expgm ValTru
ifFls = expgm ValFls
{-
## A "pure" solution

Consider the following definitions of functions to translate programs,
statements and guards.  The list of unused new names must be passed
around, or "threaded through" the computation.
-}
translate' :: (Show a) => [a] -> N         -> M
translate' ns (N source) = M (transS' (name <$>ns) source ++ [Halt])
{-
We only give two cases: the null statement and the translation of an "if" statement.  Translating an "if" statement
involves two names, to be used as labels for jumps, and splitting the
rest of the labels up for the translation of the guard, the
translation of the "then" statement, and the translation of the "else"
statement.
-}
transS' :: [String]        -> Stmt           -> [Instr]
transS'    _                  Skip            = [] -- nothing to do!
transS'    _                  (v := ValInt n) = [MovI v n]
transS'   (els:end:remainder) (Ifte g p q)    =
        transGuard' gnames g els
     ++ transS' pnames p
     ++ [JumpL end,
         Label els]
     ++ transS' qnames q
     ++ [Label end]
  where
    (gnames, pnames, qnames) = split3 remainder
transS'  names                s             = undefined -- other cases omitted

split3 :: [a]         -> ([a], [a], [a]) -- splits an infinit list into 3
split3    (x:y:z:rest) = (x:xs, y:ys, z:zs)
  where
    (xs, ys, zs) = split3 rest

transGuard' :: [String] -> Expr -> String -> [Instr]
-- code to jump to "l" if "g" is false, otherwise falls through,
-- and uses "names" as necessary
transGuard'    _           ValTru  _  = [] -- fall through
transGuard'    _           ValFls  l  = [JumpL l] -- jump to label "l"
transGuard'    names       g       l  = undefined -- other cases omitted
{-
We can now translate our example N programs:
-}
ex1', ex2' :: M
ex1' = translate' [0..] ifTru
ex2' = translate' ['A'..] ifFls
{-
## A monadic solution

An alternative is to use a monad to generate fresh names.

We need an updateable state, containing the next label, and a value to
return to the computation at hand.  We could use a pair, but it is
convenient to introduce a new type, `Content`.
-}
data Content s a = Content {value :: a, state :: s}
  deriving (Show)
{-
It might seem as though `Content` has all the information we need.  We
can instantiate `Content` as a `Functor`, which we do below, but the
problem comes when we try and instantiate it as an `Applicative`, and
need a definition for `pure`.  The expression `pure v` can install `v`
as the value to be returned, but how do we choose a state to pair with
it?  The solution is to have a more involved data structure, that can
provide an appropriate state as needed: a function with a state as
input and a `Content` carrying that state as output.  This gives us
our final definition of the type we will use, `Fresh`.
-}
newtype Fresh s a = Fresh {(/$/) :: s -> Content s a}
{-
To instantiate `Fresh` as a monad we need to first to instantiate it
as a functor, and then as an applicative.  To `fmap` a value of type
`Fresh a`, we just need to apply the function to the returned
component (think of this as a virus, with content `f`, being carried
by a protective sheath `fmap` that injects it into the structure,
modifying part of the structure).  We instantiate `Content` as a
functor to help us.
-}
instance Functor (Content s) where
  -- fmap :: (a -> b) -> Content s a -> Content s b
  fmap f (Content x s) = Content (f x) s

instance Functor (Fresh st) where
  -- fmap :: (a -> b) -> Fresh s a -> Fresh s b
  fmap g = Fresh . fmap (fmap g) . (/$/)
{-
`fmap g` over `Fresh` extracts the value (a function) from its
(`Fresh`) argument using `(/$/)`, processes it using `fmap (fmap g)`,
and then replaces it into the world of `Fresh` using `Fresh`.  In the
expression `fmap (fmap g)` the outer `fmap` is over functions, and the
inner one is over `Content`; together they push `g` to the position
where it is to be applied.  We could have used the `(<$>0` operator
and written `((g<$>)<$>)` for `fmap (fmap g)`; use whichever is
clearer at the time.

We now turn to instantiating `Fresh` as an applicative.  We need to
define `pure` and `(<*>)`.

To instantiate `Fresh` as an applicative we need to define a `pure` so
that places a `Fresh` coat around a function containing the value in
its output.
-}
instance Applicative (Fresh s) where
  -- pure :: a -> Fresh s a
  pure     = Fresh . Content
{-
We can read this as `pure` embeds its argument as `Content` in
`Fresh`.

To `apply` a fresh value to a fresh datum we need to extract the
underlying function and datum.  It is important that we sequence these
extractions; sequencing is achieved by explicitly extracting the datum
from the state reached by extracting the function.  Then we need to
reassemble the parts back into a `Fresh` value.
-}
  -- (<*>) :: Fresh s (a -> b) -> Fresh s a -> Fresh s b
  Fresh fg <*> Fresh fx                      = Fresh h
    where
      h z = Content (g x) z''
        where
          Content g z'  = fg z
          Content x z'' = fx z'    
{-
Note that:
```haskell
g   == (value              . fg) z
x   == (value . fx . state . fg) z
z'' == (state . fx . state . fg) z
```
Both `x` and `z''` are found by sequencing the extraction of the
components, `g` before `x`.

Finally we instantiate the monad, by defining the operator `(>>=)`.
Again, we need to extract a value from its `Fresh` coat, and then
reassemble a `Fresh` value from an application.
-}
instance Monad (Fresh a) where
  -- (>>=) :: Fresh a -> (a -> Fresh a) -> Fresh b 
  (Fresh x) >>= gf                       = Fresh h
    where
      h z = gf x' /$/ z'
        where
          Content x' z' = x z
{-
Now we can implement a monadic value to deliver the next fresh name.
It needs its state to be both enumerable, so that the `succ`essor
value is defined, and showable, so that it can be turned into a
string.  In practice we will instantiate `s` as `Int` or `Integer` so
that we have a lot of values. `Bool` is possible instantiation, but
that would give us only two fresh names!

We can structure the definition as a functoral mapping of `name` over
a simple monadic value that increments its state, reporting the
original value.
-}

next :: (Enum s) => Fresh s s
next = Fresh h
  where
    h z = Content z (succ z)

fresh ::  (Enum s, Show s) => Fresh s String
fresh = name <$> next
{-
We can use `fresh` as follows:
-}
translate :: (Enum a, Show a) => a -> N -> M
translate firstlabel (N s)
  = M (   value (transS s /$/ firstlabel)
       ++ [Halt])

transS :: (Enum a, Show a) => Stmt -> Fresh a [Instr]
transS Skip = return [] -- nothing to do
transS (v := ValInt n) = return [MovI v n] -- constant assignment
transS (Ifte g p q) =
  do
    els <- fresh
    mg <- transGuard g els -- jump to "els" if "g" false, otherwise fall through
    mp <- transS p
    mq <- transS q
    end <- fresh
    return (   mg
            ++ mp
            ++ [JumpL end]
            ++ [Label els]
            ++ mq
            ++ [Label end])
transS s    = undefined -- other cases omitted

transGuard :: (Enum a, Show a) => Expr -> String -> Fresh a [Instr]
 -- generate jump to "l" if guard false, otherwise fall through
transGuard ValTru _ = return [] -- fall through
transGuard ValFls l = return [JumpL l] -- jump to label l
transGuard g      l = undefined -- other cases omitted
{-
Finally we repeat the two previous examples in the new setting.  Note
how we have swapped a list (`[0..]`, `['a' .. 'z']`) for an
`Enum`eration.  Note that `translate'` has the problem that it could
be provided with a list that contains repetitions, such as `repeat 0`.

**Exercise** rewrite `translate'` to have the identical interface to
`translate`.  The difference between this version of `translate'` and
`translate` is centralised use of `Enum` _versus_ distributed use of
`Enum`.
-}
ex1, ex2 :: M
ex1 = translate 0 ifTru
ex2 = translate 'A' ifFls
{-
## A more complete compiler

Seperately we have provided a complete development of a compiler, that
includes a translation for each statement of N.  Rather than use a
hand-built monad, as here, the translator uses the library
`Control.Monad.State.Lazy` to provide the monad `State Int` (`Int`, as
this or `Integer` are really the only sensible choices from `Prelude`
for generating labels!).

The compiler includes modules for the following components:
* For the high-level language, N:
  * a lexical scanner (converts strings to lists of tokens)
  * a parser (converts lists of tokens to an abstract syntax tree [AST])
  * a pretty printer (converst an AST to a string)
  * a static checker (checks types and more)
  * a simplifier (converts an AST to a simpler AST)
  * an interpreter (runs a statically-correct AST)
* For the low-level language, M:
  * an interpreter (runs an AST)
  * a global optimiser (converts an AST into a simpler AST)
  * a local optimser (converts an AST into a simpler AST)
  * a register allocator (converts an AST using any number of
    arbitrary register names to one using a fixed number of standard
    register names, using external memory, where necessary)
* A translator (converts an N AST into an M AST)

It does not include a compiler constructed from the parts (yet).
-}
