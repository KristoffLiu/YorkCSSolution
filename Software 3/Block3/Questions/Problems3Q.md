```haskell
module Problems3QA where
import Data.Monoid

```
# SOF3/Block 3
```haskell
```
# Problems
```haskell

```
## Q1: Extending `Monoid` to `Group`

In abstract algebra, the concept of **monoid** is a step towards the
important concept of **group**. (Groups have a use in quantum theory,
and so are important in the study of quantum computation, which can be
studied in "Quantum Computation" (QUCO) in Stage 3 or 4.)
* A **semigroup** is an **associative** binary operator over a type.
  It is represented in Haskell by the type class [`Semigroup a`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Semigroup), with
  an operator `(<>) :: a -> a -> a` that should satisfy:
  `∀ x, y, z :: a {x <> (y <> z) == (x <> y) <> z}`.
* A **monoid** is a semigroup with an **identity** element.  It is
  represented in Haskell by the type class [`Monoid a`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Monoid), where
  `Semigroup a` must hold, providing an operator `(<>)`; in addition
  it provides an element `mempty::a` that should satisfy:
  `∀ x :: a {mempty <> x == x && x <> mempty == x}`.
* A **group** is a monoid with a notion of **inverse**.  It is not
  represented in the Haskell standard libraries.  This exercise asks
  you to create one, with a function `ginverse :: a -> a` that should
  satisfy:
  `∀ x :: a {(ginverse x <> x) == mempty && (x <> ginverse x) == mempty}`.

Create a type class, `Group`, with suitable fields.
```haskell
```
The [`newtype Sum a`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:Sum) is a renaming of `Num` types `a` that form a `Semigroup` with `(<>) == (+)` and a `Monoid` with `mempty == 0`.

Instantiate `Sum a` as an instance of the type class `Group`.
```haskell
```
> We could do something similar for the type class `Num a => Product
> a`, but it is far more problematic in practice, as there are two
> types of division, one for floating point numbers and one for
> integral numbers.

## Q2 The Group of Equilateral Triangle symmetries

Many important groups arise from the operations that represent
symmetries of geometric figures.  The smallest of these is for an
equilateral triangle.

There are two types of symmetric transform on an equilateral triangle:
three _rotations_ and three _reflections_.  Here is one example of each:
```
  a                    c                a                  a
  /\   - Rotn 120 ->   /\               /\   - Refl X ->   /\
c/__\b               b/__\a           c/__\b             b/__\c
``` 
The operations can be captured by a data type:
```haskell
data Turn = Nought | OneTwenty | TwoForty -- angles in degrees
  deriving (Eq, Enum, Show)
data Axis = X | Y | Z -- X top vertex, Y bottom right vertex, Z bottom left vertex
  deriving (Eq, Enum, Show)
data EqTriSym = Rotn Turn -- rotations
              | Refl Axis -- reflections
  deriving Show
```
Translations can be combined, by first doing one and then a second.
Each ordered pair of transformations corresponds to a single
transformation, and we can capture this as a function, `(|>)`:
```haskell
(|>) :: EqTriSym -> EqTriSym -> EqTriSym
Rotn a |> Rotn b = Rotn (a `turnTurn` b)
  where
    turnTurn a b = toEnum ((fromEnum a + fromEnum b) `mod` 3)
Refl a |> Refl b = Rotn (a `axisAxis` b)
  where
    axisAxis a b | a == b                                 = Nought
                 | toEnum ((fromEnum a + 1) `mod` 3) == b = TwoForty
                 | otherwise                              = OneTwenty
Rotn a |> Refl b = Refl (a `turnAxis` b)
  where
    turnAxis a b = toEnum ((fromEnum a + fromEnum b) `mod` 3)
Refl a |> Rotn b = Refl (a `axisTurn` b)
  where
    axisTurn a b =  toEnum ((fromEnum a + fromEnum (neg b)) `mod` 3)
```
Here the utility `neg` is:
```haskell
neg :: Turn     -> Turn
neg    Nought    = Nought
neg    OneTwenty = TwoForty
neg    TwoForty  = OneTwenty
```
Instantiate `EqTriSym` as a `Semigroup`, a `Monoid` and a `Group`.
```haskell
```
## Q3 `All` and `Any`

The Haskell base libraries include, in `Data.Monoid`, two renamings of
`Bool` instantiated as `Monoid`: `All` and `Any`.  These are used to
implement two functions in `Prelude` with types:
```haskell
all, any :: Foldable t => (a -> Bool) -> t a -> Bool
```

The expression `all p xs` returns `True` exactly when every element in
`xs` satisfies `p`, and `False` otherwise; `any p xs` returns `False`
when every element in `xs` satisfies `not . p`.  Implement your own
versions: `all'` and `any'` using `foldMap` and the monoids
[`All`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:All)
and
[`Any`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Monoid.html#t:Any).
You will need the accessors `getAny :: Any -> Bool` and `getAll :: All
-> Bool` as well as the constructors, `Any :: Bool -> Any` and `All ::
Bool -> All`.
```haskell
all', any' ::  Foldable t => (a -> Bool) -> t a -> Bool
all' = undefined
any' = undefined
```
The functions `all'` and `any'` have the same structure.  You are
going to write a function, `compact` that abstracts this structure.
The function `compact` can be used to create versions of `all` and
`any`:
```haskell
all'', any'' :: Foldable t => (a -> Bool) -> t a -> Bool
all'' = compact All
any'' = compact Any
```
The function relies on being able to extract a `Bool` from a monoid
over `Bool`.  To do this first define a class `Extractable` that has
one method, `extract :: a -> Bool`.  Then instantiate it for `Any` and `All`.
```haskell
```
Now write the function `compact`, using the fact that a suitable
function `extract` is available.
```haskell
compact :: (Foldable t, Monoid b, Extractable b) => (Bool -> b) -> (a -> Bool) -> t a -> Bool
compact fromB p = undefined -- expression using `extract`
```
## Q4 Reasoning about `Nat`

This question involves proof.  For convenience, here is a repeat of
the `ProofLayout` type and the definition of `testPL`.
```haskell
infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show
instance Foldable ProofLayout where
  foldr f z = ffz
    where
      ffz QED        = z
      ffz (p :=: pl) = f p (ffz pl)
testPL :: Eq a => ProofLayout a -> Bool
testPL QED        = True
testPL (p :=: pl) = all (==p) pl
```
Recall the recursive data type:
```haskell
data Nat = Zero | Succ Nat deriving (Eq, Show)
oneN, twoN, threeN :: Nat
oneN   = Succ Zero -- oneN.0
twoN   = Succ oneN -- twoN.0
threeN = Succ twoN -- threeN.0
```
This data type has exactly the same structure as that of the natural
numbers: they both obey [the Peano axioms](https://brilliant.org/wiki/peano-axioms/),
one of which is the axiom of induction.

`Nat` is essentially a _unary_ encoding of natural numbers.

### Q4.1 Operations in `Nat`
Define functions to add, multiply and square elements of `Nat`
```haskell
(/+/), (/*/) :: Nat -> Nat -> Nat
sqn :: Nat -> Nat
(/+/) = undefined
(/*/) = undefined
sqn   = undefined
```
### Q4.2 Unit of multiplication
Prove that: `∀ n::Nat {oneN /*/ n == n}`.
```haskell
unitMul :: Nat -> ProofLayout Nat
unitMul = undefined
```
### Q4.3 Summation of odd numbers

Given a positive `Nat` (that is, for any value not `Zero`) define a
function that directly encodes the summation of the odd numbers from
`1` up to `2*n - 1`, where the numbers are encoded as encoded as a
`Nat`:
```haskell
let Succ m = twoN /*/ n in -- encodes m == 2*n - 1 
  oneN /+/ threeN /+/ ... /+/ m
```

**Hint** rather than trying to encode subtraction, define the function
in two cases
1. One that deals with `n==1` (encoded as a `Nat`).
2. One that deals with larger values of `n==k+1`, for positive `k`,
   and rewrite the formula for the final value in terms of `k` (all
   encoded as values in `Nat`).
```haskell
sumOdd :: Nat -> Nat
sumOdd = undefined
```
### Q4.4 An induction proof over `Nat`
We can prove theorems by induction over `Nat`.  Given a predicate
`p::Nat->Bool` the induction rule is:
> If `p Zero` and `∀ n::Nat {p n ⇒ p(Succ n)}` then `∀ n::Nat {p n}`

Prove by structural induction the theorem from THE1, "Practical Sheet
for Lectures 4, 5, & 6", Q2, which can be recast as
```haskell
∀ n :: Nat {sumOdd (Succ n) == sqn (Succ n)}
```
or as the even terser
```haskell
sumOdd . Succ = sqn . Succ
```
You may assume that `∀ n::Nat {(n+1)^2 == n^2 + 2*n + 1}`, which is encoded as: 
```haskell
sqSucc :: Nat -> ProofLayout Nat
sqSucc n = sqn (Succ n) :=: sqn n /+/ (twoN /*/ n) /+/ oneN :=: QED
```
(For further practice, you may prove `sqSucc`.)
```haskell
sumOdd_Sq :: Nat -> ProofLayout Nat
sumOdd_Sq = undefined
