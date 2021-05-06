```haskell
module TypeSys where

```
# Jeremy Jacob, 17 Sep 2020
# The Haskell type system

Getting the types right is an important way of:
1. documenting code, and
2. getting the Haskell system to protect you from a large class of
mistakes & errors.

## Built-in types

Haskell has very few built-in types.  Essentially only the numeric
types and characters are built in, for efficiency.  Everything else
can be defined in the language itself, even the type of `Bool`ean
values.  The only built-in support for types outside of these are
functions, such as `if...then...else...` and type constructors, such
as `[...]`, with a non-standard syntax.

The type of characters is called `Char`, and consists of "an
enumeration whose values represent [Unicode](http://www.unicode.org/)
(or equivalently ISO/IEC 10646) code points".  Individual characters
are represented using single-quotes, for example: `'a'`, `'A'`, `'0'`,
`'&'`.  In addition there are special symbols, such as `'\n'` for a
newline character.

The numeric types are quite complex.  While they follow a standard
mathematical classification, they also need to take into account the
limits of finite machines.  Two will be important for our purposes:
1. `Int`: a bounded sub-range of the Integers "with at least the range
`[-2^29 .. 2^29-1]`. The exact range for a given implementation can be
determined by using `minBound` and `maxBound` from the `Bounded`
class."  On my machine these values are `-9223372036854775808` and
`9223372036854775807` respectively.
2. `Integer`: "Arbitrary precision integers. In contrast with
fixed-size integral types such as Int, the Integer type represents the
entire infinite range of integers."  In practice, you may fill all of
available memory with an integer.

Values of both kinds are represented in the same way, for example `1`,
`42`, `9876543210`, and so on.  Care is needed with the literal
representation of negative numbers; a prefixed negation sign, `-` (for
example, `-42`) usually works, but sometimes Haskell cannot parse this
properly as it confuses the negation sign with the subtraction
operator, `(-)`.  In such a case, use brackets (for example,
`(-42)`) or the explicit negation function (for example, `negate
42`).

Generally `Int` will be more efficient than `Integer`, but in some
applications `Integer` will work when `Int` fails.

## Standard defined types

[**Note** Parts of this section will make more sense after reading the
next section.]

Haskell provides, among many others, the following types in its
standard library, `Prelude`.  These could all be left to the
programmer to define if needed, but this predefinition means that
libraries of utilities can be provided, and compilers can provide
targeted optimisations.

* `()` The _unit_ type, with exactly one member, also written `()`.
```haskell
data () = ()
```
* `(a, b)` The type of pairs, whose first element is of type `a`, and
  whose second element os of type `b`.  For example, `(Int, Char)`
  includes the value `(3, 'c')`.
```haskell
data (a, b) = (a, b)
```
Tuple types exist with up to 62 elements.

* `Bool` The type with `True` and `False` as its only members
```haskell
data Bool = False | True
```
* `[a]` Lists whose elements are drawn from type `a`.  For example
  `[Int]` contains the value `[2,3,5,7,11]`.
```haskell
data [a] = [] | a : [a] -- this syntax is not available to the programmer
```
* `String` A synonym for `[Char]`, lists of characters.
```haskell
type String = [Char]
```
* `Either a b` The type that represents the sum (disjoint union) of
  types `a` and `b`.
```haskell
data Either a b = Left a | Right b
```
* `Maybe a` The type that extends type `a` by a single, special value.
```haskell
data Maybe a = Nothing | Just a
```
This type is isomorphic to the sum of the type `a` and the unit type:
`Either () a`.

All these types are members of type classes `Eq`, `Ord`, `Read`, and
`Show`, as well as others, conditional on their parameter types, where
appriopriate, also being in the same class.  `Bool` is also a member
of type class `Enum`.

## New types

The first way of introducing a new name for a type is to use the
keyword `type`.  This gives a **synonym** for a type-expression, not a
new type.

For example, a record about a person may include details such as their
age and house number.  We could introduce names to record this:
```haskell

type Age' = Int
type HouseNumber' = Int

```
Type names **must** start with a capital letter.

Suppose Alice is aged 42 and lives at house number 16:
```haskell

aliceAge' :: Age'
aliceAge' = 42
aliceHN' :: HouseNumber'
aliceHN' = 16

```
This way of introducing new type namesis **NOT** recommended in
general.  All `type` does is to provide documentation of _intent_.

It it does not protect us against writing expressions which disregard
the intent, such as:
```haskell

silly1 :: Bool
silly1 = aliceAge' * aliceHN' == aliceAge' + aliceHN'

```
Because Haskell cannot tell the difference between `Age'` and `Int` we
cannot, for example, define `show :: Age' -> String` without clashing
with the pre-existing definition `show :: Int -> String`.

An advantage of `type` is that it does not introduce any extra
overhead at runtime.

**RECOMMENDATION** Use `type` when the concept captured by the name is
**identical** to its meaning

Haskell provides two ways of introducing genuine new types, using the
keywords:
1. `newtype` -- when the new type has an isomorphic value space to the
    type expression
2. `data` -- all other types

Here is a better (but not yet our final) way of introducing `Age` and
`HouseNumber`
```haskell

newtype Age'' = Age'' Int
newtype HouseNumber'' = HouseNumber'' {hn :: Int}

```
As well as introducing a new type name these declarations also each
introduce a compulsory **constructor**, which is a function that
converts an `Int` to an `Age''` or `HouseNumber''` as appropriate.

Constructor names **must** start with a capital letter, but are
otherwise identical to ordinary variable names.  They may be the same
as the type name, or they may be different.

**RECOMMENDATION** In `newtype` definitions make the constructor and
type names the same.

Here is how we could define Alice's age & house number:
```haskell

aliceAge'' :: Age''
aliceAge'' = Age'' 42
aliceHN'' :: HouseNumber''
aliceHN'' = HouseNumber'' 16

```
`HouseNumber''` also introduces an _inverse_ function, also called an
_accessor_ function, `hn :: HouseNumber -> Int`; `Age''` has no
inverse function.  Inverse functions are less useful than you might
think, because of the ability to define functions by patterns.  In
other languages they are called "accessor" functions, or "fields", or
"deconstructors".

Now the expression `aliceAge'' * aliceHN'' == aliceAge'' + aliceHN''`
contains several type errors.  If we really need to write it we can,
using the various ways of computing inverses that we have at our
disposal:
```haskell

silly2 :: Bool
silly2 = aa * ahn == aa + ahn
  where
    Age'' aa = aliceAge'' -- on-the-fly inverse using a pattern
    ahn = hn aliceHN'' -- inverse using predfined function

```
This mechanism gives us **explicit coercion** between types, always
_much_ safer than implicit coercion.

The run-time implementation of `newtype` is as efficient as the
run-time implementation of `type`.

The _only_ functions we have at our disposal are: `Age''::Int->
Age''`, `HouseNumber''::Int->HouseNumber''`, and
`hn::HouseNumber''->Int`.  There is, for example, no function
`(==) :: Age'' -> Age'' -> Bool`, so we need to define one.

The equality and inequality, `(/=)::Age''->Age''->Bool`, functions are
part of the **type class** `Eq`.  (Type classes are a bit like
_interfaces_ in a language such as Java.)  We could define these
functions outside of the type class mechanism, but then Haskell would
not know what their purpose was.  Haskell can use the information that
`Age''` is an **instance** of the `Eq` type class.
```haskell

instance Eq Age'' where
  (Age'' m) == (Age'' n) = m == n

```
We do not need to explicitly define `(/=) :: Age'' -> Age'' -> Bool`.
We can define either operator, choosing whichever is most convenient,
and Haskell will work out the definition of the other for itself.

In some very rare cases it can be more efficient to define both, _but_
the programmer is responsible for ensuring the property
`(a /= b) == not (a==b)`
```haskell

example3 :: String
example3 = (if aliceAge'' == Age'' 18 then "F" else "Not f")
           ++ "irst adult year"

```
The definition of `(==)::Age''->Age''->Bool` is exactly what we might
expect: two ages are the same if their underlying integers are the
same.  In this situation we only need to instruct Haskell to
**derive** all the definitions for type class `Eq` itself.

There are many other type classes which are appropriate for ages, such as:
* `Ord` -- ordering relations `(<)`, `(<=)`, and so on
* `Show` -- conversion to String (useful for printing) via `show::Age->String`

We can derive all of these together, in our final definition of `Age`:
```haskell

newtype Age = Age Int deriving (Eq, Ord, Show)

aliceAge :: Age
aliceAge = Age 39

example4 :: String
example4 = (if aliceAge < Age 18 then "Not a" else "A") ++ "dult"

```
Because `Age` is an instance of `Ord` we have functions such as `sort
:: [Age] -> [Age]` automatically defined.

A useful type class for `Age` is `Enum`, which has a notion of next
age, `succ`, previous age, `pred`, and age ranges.  Haskell does not
have enough information to derive these, so we need to do this for
ourselves in an instance declaration.

We can do this by defining two functions to coerce `Age` to and from
`Int`.  In this case suitable definitions are immediate, even though
Haskell cannot see this.
```haskell

instance Enum Age where
  -- toEnum :: Int -> Age
  toEnum = Age
  -- fromEnum :: Age -> Int
  fromEnum (Age n) = n

```
These two functions must be inverses of each other:
1. `toEnum . fromEnum == id :: Age -> Age`
2. `fromEnum . toEnum == id :: Int -> Int`

Here are the proofs, by simple equational reasoning,
with hints as to which definition is used at each step:

Proof of (1):

`  (toEnum . fromEnum) (Age n)`

`== -- (.)`

`  toEnum (fromEnum (Age n))`

`== -- fromEnum`

`  toEnum n`

`== -- toEnum`

`  Age n`

`== -- id`

`  id (Age n)`

Proof of (2):

`  (fromEnum . toEnum) n`

`== -- (.)`

`  fromEnum (toEnum n)`

`== -- toEnum`

`  fromEnum (Age n)`

`== -- fromEnum`

`  n`

`== -- id`

`  id n`

We can now define, for example:
```haskell

teenage, retirementYears :: [Age]
teenage = [Age 13 .. Age 19]
retirementYears = [Age 66 ..] -- an infinite list!

```
For completeness here is another definition for `HouseNumber`.

**DISCUSS** Why is it inappropriate to derive `Ord`, or instantiate
`Enum`?
```haskell

newtype HouseNumber''' = HouseNumber''' Int deriving (Eq, Show)

```
For full generality when defining new types we use the `data` keyword.
This allows multiple constructors, with any number of types, including
none.  It does have a cost at run-time, as the constructors need to be
explicitly represented.

The first example defines a type `Day`, with seven arity-zero
constructors:
```haskell

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Eq, Ord, Enum, Show)

```
Type class `Enum` includes functions `succ` and `pred` that are almost
`tomorrow` and `yesterday`, except that `succ Sat` and `pred Sun` are
undefined.  We can use these to create `tomorrow` and `yesterday`.
```haskell

tomorrow :: Day -> Day
tomorrow    Sat  = Sun    -- explict about `tomorrow Sat`
tomorrow    d    = succ d -- use `succ` for all other days

```
Enum types also allow ranges
```haskell

weekday :: [Day]
weekday  = [Mon .. Fri]

```
**EXERCISE** define `yesterday :: Day -> Day`

House identifiers are more complex than just a number.  Some houses
have a name and no number.  Some have a number and no name.  Some have
both.  Sometimes the number can have a letter added to it.  A `data`
definition can cope with this. Here is _one_ way of structuring the
data:
```haskell

data HouseID
  = Name String -- constructor with one parameter
  | Number Int -- ... one parameter
  | SubNumber Int Char --  ...two parameters
  | NameNumber String Int -- ...two parameters
  | NameSubNumber String Int Char -- ...three parameters
  deriving (Eq, Show)
 
```
Example `HouseID`s
```haskell

dr, no16, no16a, drno16a :: HouseID
dr      = Name "Dunroamin"
no16    = Number 16
no16a   = SubNumber 16 'a'
drno16a = NameSubNumber "Dunroamin" 16 'a'

```
`data` definitions look a bit like BNF definitions.  This is **not** a
coincidence.

As another example, here is a definition of types to describe a
miniscule imperative programming language, called "N".  This is often
called **an abstract syntax**.

Constructor names may also be symbolic.  If so they must start with a
colon '`:`'.  We also explicitly give them a fixity and priority
(higher numbers mean tighter binding).
```haskell

type Var = String -- Alias for variable names

infixr 4 :> -- sequential composition
infix  5 := -- assignment
infix  6 :<: -- less than
infix  6 :=: -- equality
infixr 7 :+: -- summation
infixr 8 :*: -- multiplication
infixr 8 :=>: -- implication

newtype N = N Stmt deriving (Show)

-- abstract syntax of statements
data Stmt = Skip                 -- null statement
          | Print Expr           -- Print value of Integer expression
          | Var := Expr          -- Assignment
          | Stmt :> Stmt         -- sequential composition
          | Ifte Expr Stmt Stmt  -- if then else
          | While Expr Stmt      -- while do
          | Block Var Type Stmt  -- declaration
          deriving (Eq, Show)

-- abstract syntax of types (a trivial type system)
data Type = IntType | BoolType deriving (Eq, Show)

-- abstract syntax of expressions
data Expr = Ref Var    -- variable name
          | ValInt Int -- integer value
          | ValTru     -- Boolean value
          | ValFls     -- Boolean value
          | Expr :+: Expr -- add
          | Expr :*: Expr -- times
          | Neg Expr        -- unary negation
          | Expr :=>: Expr -- implication
          | Expr :=: Expr -- equality
          | Expr :<: Expr -- inequality
          deriving (Eq, Show)

```
We will look at various functions that takes values of these types
later.  These may include: simplification, typechecking & compilation
depending on time.

Here is a value of type `N`, that represents a program that prints the
sum of the first 10 numbers using the naive algorithm.
```haskell

sumUpTo10 :: N
sumUpTo10 = N
            (Block "sum" IntType
             ("sum" := ValInt 0
             :> Block "i" IntType
                (  "i" := ValInt 1
                :> While (Ref "i" :<: ValInt 11)
                          (  "sum" := Ref "sum" :+: Ref "i"
                          :> "i" := Ref "i" :+: ValInt 1)
                :> Print (Ref "sum"))))


```
So far all our new types are constant types: but they can have
parameters.

Our first example allows an annotation string to be attached to a
value.
```haskell

type Annotated' a = (a, String)

carloAge' :: Annotated' Age
carloAge' = (Age 18, "D.O.B. 2003-Jan-01")

sportsDay' :: Annotated' Day
sportsDay' = (Wed, "Afternoon only")

```
Better, of course is a `newtype`
```haskell

newtype Annotated a = Annotated (a, String) deriving (Eq, Show)
carloAge :: Annotated Age
carloAge  = Annotated (Age 18, "D.O.B. 2003-Jan-01")

sportsDay :: Annotated Day
sportsDay  = Annotated (Wed, "Afternoon only")

```
Here is a `data` definition, describing binary trees

A binary tree is either
* an empty leaf, or
* a node containing a left tree, a value and a right tree

```haskell

data BinTree a = Leaf | Node (BinTree a) a (BinTree a)
  deriving (Eq, Show)

```
Examples
```haskell

sof3t :: BinTree Char
sof3t = (Node (Node Leaf -- layout shows tree structure
               'S'
                   Leaf)
         'O'
              (Node Leaf
               'F'
                    (Node Leaf
                     '3'
                           Leaf)))

weekdayt :: BinTree Day
weekdayt = (Node Leaf
            Mon
                (Node Leaf
                  Tue
                      (Node Leaf
                       Wed
                            (Node Leaf
                             Thu
                                  (Node Leaf
                                   Fri
                                        Leaf)))))

```
We can define functions without knowing the embedded type of the tree
```haskell

height :: BinTree a          -> Int
height    Leaf                = 0
height    (Node left _ right) = (height left `max` height right) + 1

```
We can also find the left- and right-most values.  These functions
fail if applied to leaves!
```haskell

leftmost, rightmost
       :: BinTree a       -> a
leftmost  (Node Leaf v _)  = v
leftmost  (Node left _ _)  = leftmost left
rightmost (Node _ v Leaf)  = v
rightmost (Node _ _ right) = rightmost right

```
If we know that the embedded type is ordered we can check if a tree is ordered
```haskell

ordered :: Ord a
  =>    BinTree a          -> Bool
ordered Leaf                = True
ordered (Node Leaf v Leaf)  = True
ordered (Node Leaf v right) = v <= leftmost right && ordered right
ordered (Node left v Leaf)  = rightmost left <= v && ordered left
ordered (Node left v right) =    rightmost left <= v && v <= leftmost right
                              && ordered left && ordered right
