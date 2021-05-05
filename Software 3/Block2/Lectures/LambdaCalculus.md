```haskell
module LambdaCalculus where
```
# Lambda calculus, logic, and the origins of functional programming
# Jeremy Jacob 28 Oct 2020

## Learning outcomes

1. Understand history and deep background to functional programming
   languages.
2. Understand how to embed a very simple language

## History

Back in the early part of the 1900s mathematicians and logicians were
interested in the possibility of computing or calculating the answers
to problems, as they hoped that, following David Hilbert, all problems
that could be encoded in mathematics could be solved by calculation.

A number of things came out of this. including:
1. **Kurt Gödel** proved that the answer to some problems could *not*
   be calculated, by proving his famous incompleteness theorem.
2. **Gerhard Gentzen** developed the two most popular forms of
   presenting logics: Natural Deduction and Sequent Calculus.
3. **Alan Turing** developed his ideas about the underlying theory of
   computation (see THE3), including the idea of the Turing machine.
   As part of this he proved an analogue of Gödel's incompleteness
   theorem: the existence of incomputable functions.
4. **Alonzo Church** developed a very simple way of expressing the
   idea of a computation in order to study the subject: the _lambda
   calculus_.  One of Church's students was Alan Turing who was
   exploring Turing machines.  Kurt Gödel was a collaborator, and
   developed a third way of expressing the idea of computation:
   recursive function theory.  The Church-Turing thesis (that all
   models of computation are equivalent, and in particular, equivalent
   to a Turing Machine) came out of the interaction of the three
   researchers.
5. **Haskell B. Curry** developed combinatory logic, for similar
   reasons to Church's development of lambda calculus.  With William
   A. Howard, he was part of the development of the Curry-Howard
   isomorphism which shows how logical systems such as Gentzen's are
   the identical to computational systems such Turing's and Church's,
   just presented in a very different way.

Philip Wadler (one of the designers of Haskell, and a major
contributer to programming language theory and practice) has a nice
talk about the Curry-Howard isomorphism.  There are several versions
available on YouTube, such as [that given at the Strange Loop
conference in 2015][2].

In effect the lambda calculus was the world's first programming
language, devloped about a decade before the first electronic
computer.  Declarative programming structured around functions grew
out of the lambda calculus and combinatory logic.  (A different kind
of declarative programming, structured around logical formulæ, grew
out of the ideas of Gödel and Gentzen, giving rise to languages such
as `Prolog`.  Imperative programming grew out of manipulating the
early electronic computers; the designer of FORTRAN (one of the first
really successful imperative languages), John W. Backus, realised the
inherant problems with this and his [1977 Turing award lecture][1]
presented a design for a functional programming language.

Hence it is useful to understand the lambda calculus to help
understand the underpinings of functional programming.  It will also
give us an excuse to look at an example of developing language tools
in Haskell.

## The Lambda Calculus

### Introduction

A Lambda Calculus _expression_ has one of three forms:
1. **Names**, which are atomic.
2. **(Function) Abstractions**, which consist of an atomic name and an
   expression, and represent a function whose input is the name and
   whose expression is the output.  To distinguish these from the
   other two types of expression, Church prefaced them by a 'λ' (the
   lowercase form of the Greek letter "lambda"): `λ v e`.
3. **Applications**, which consist of two expressions, one (we will
   take the first) as representing a function, and the other (the
   second) as representing the data supplied to the function: `f d`.

We can easily encode this in Haskell, as a `data` type.  We could
decide that names are a fixed type, but, as they are atomic we may
take them as a parameter to the type.

[As we will also need the concept of sets we import a library for
sets, based on balanced binary trees.]
```haskell

import qualified Data.Set as S

data LExp a = Name a
            | Lambda a (LExp a)
            | App (LExp a) (LExp a)
  deriving Show

```
Examples are:
* `Name 'x'` of type `LExp Char`
* `Lambda 0 (App (Name 3) (Name 4))` of type `Num n => LExp n`
* `App (Lambda "param" (App (Name "param") (Name "const"))) (Name "func")`
   of type `LExp String`

And we can pretty-print lambda expressions, to look a little more like
standard notation:
```haskell

pp :: Show a => LExp a -> IO ()
pp = putStrLn . pp'
  where
    pp' (Name v) = show v
    pp' (Lambda v e) = "(λ" ++ show v ++ " " ++ pp' e ++ ")"
    pp' (App f d) = '(': pp' f ++ " " ++ pp' d ++ ")"

```
The above three expressions pretty print as:
* `'x'`
* `(λ0 (3 4))`
* `((λ"param" ("param" "const")) "func")`

### Computation in the Lambda Calculus

The notion of computation in the Lambda calculus is to simplify
applications until no further simplifications are possible.  A single
simplification step is known as _β-reduction_ ('β' is the lowercase
form of the Greek letter "beta").  For example, `(λ v (v d)) (λ w w)`
is β-reduced to `(λ w w) d`, by substituting the term `(λ w w)` for
the variable `v` in the body `v d`.  One further β-reduction
substitutes the data `d` for the variable `w` in the body `w` to
obtain the term `d`.  No further β-reduction is possible, so the final
result of repeatedly β-reducing `(λ v (v d)) (λ w w)` is just `d`.

To define β-reduction we must first define substitution.  Susbtitution
replaces all occurences of a variable in an expression by a second
expression.  There are two cases that require care:
1. Substituting occurences of a variable inside a lambda abstraction
   over that variable: replacing `v` by `e` in `λ v v` is just `λ v v`
   and not `λ v e`; `λ e e` is only correct if `e` is a variable.
2. Substituting an expression into a context where one of its
   variables is "captured".  For example: substituting `f x` for `y`
   in `λ f y` should give `λ g (f x)`, where `g` is a variable not
   otherwise used, and not `λ f (f x)`.  To deal with this case we
   need the concepts of the _free_ variable set of an expression and a
   _fresh_ variable.  The free variables are those available to be
   substituted, and a fresh variable is one that does not occur
   anywhere in the expression.

If we have a term we can easily determine its set of free variables,
as long as the variables can be ordered:
```haskell

free :: Ord a => LExp a -> S.Set a
free (Name n) = S.singleton n
free (App f d) = free f `S.union` free d
free (Lambda v e) = S.delete v (free e)

```
Given a type that is ordered (so that we can represent a set as a
search tree) and enumerated (so we have the notion of a successor),
the successor of the largest element of a finite, non-empty set over
that type cannot be in the set, and so we use that as the fresh value.
```haskell

fresh :: (Ord a, Enum a) => S.Set a -> a
fresh = succ . S.findMax

```
Substitution is defined using a special case of substitution:
_α-conversion_.  This replaces the bound variable in a lambda term.
```haskell

subst :: (Eq a, Ord a, Enum a) => a -> LExp a -> LExp a -> LExp a
subst v e = sve
  where
    sve n@(Name w)     | w==v      = e
                       | otherwise = n
    sve (App f d)      = App (sve f) (sve d)
    sve a@(Lambda w b) | w==v            = a -- tricky case 1
                       | w `S.member` fe = sve (alpha nw a) -- tricky case 2
                       | otherwise       = Lambda w (sve b) -- simple case
      where
        fe = free e
        nw = fresh (fe `S.union` free b)

```
α-conversion itself uses substitution:
```haskell

alpha :: (Eq a, Ord a, Enum a) => a -> LExp a -> LExp a
alpha v (Lambda w e) = Lambda v (subst w (Name v) e)

```
Substitution is at the heart of computation.  Tony Hoare showed how
substitution in the predicate calculus underpins assignment in
imperative programming, albeit in a more complex way than substitution
in the lambda calculus underlies function application in a declarative
language.

We can now define β-reduction.  Actually we define a function that
keeps applying β-reductions as long as possible.  The tricky case is
reducing an application where the function position is itself an
application.
```haskell

beta :: (Eq a, Ord a, Enum a) => LExp a -> LExp a
beta e@(Name _)           = e -- no change
beta (Lambda v e)         = Lambda v (beta e) -- reduce body
beta (App f d)            = case bf of
                              Lambda v e -> beta (subst v bd e) 
                              _          -> App bf bd
  where
    bf = beta f
    bd = beta d
```
Also available is _η-reduction_ ('η' is the lowercase form of the
Greek letter "eta").  This is a different kind of simplification, that
allows us to drop arguments from functions.  It follows from the fact
that two functions are equal if they have equal results on equal
arguments:
`f == g = ∀x (β (f x) == β (g x))`
Note that '∀' does not correspond to a computable operation, so
function equality cannot be computed.
```haskell
eta :: (Eq a, Ord a) => LExp a -> LExp a
eta (Lambda v (App f (Name w))) | v==w && not (w `S.member` free f) = f
```
The function `eta` fails if its conditions are not met.

## Examples

As the names of our examples may clash with Haskell names we postfix
'L' to distinguish them.  We will also take the concrete type `Char`
for the type of names.

The identity function, constant function and substitution function: 
```haskell

idL, constL, substL :: LExp Char
idL = Lambda 'v' (Name 'v')
constL = Lambda 'v' (Lambda 'w' (Name 'v'))
substL = Lambda 'v' (Lambda 'w' (Lambda 'x' (App (App (Name 'v') (Name 'x'))
                                                 (App (Name 'w') (Name 'x')))))
```
Functions `idL` and `constL` have analogues in `Prelude`: `id::a->a`
and `const::a->b->a`.  Function `substL` does not have an analogue in
`Prelude`, but it is a theoretically important function; an analogue
would have a type `(a -> b -> c) -> (a -> b) -> a -> c`.

> The remarkable thing about the types of `const` and `subst` is that
> they correspond to two of the three axioms of propositional
> calculus:
> * `a ⇒ (b ⇒ a)`
> * `(a ⇒ (b ⇒ c)) ⇒ ((a ⇒ b) ⇒ (a ⇒ c))`
>
> where `a`, `b`, and `c` stand for arbitrary propositions.
>
> The third axiom of propositional calculus deals with the proposition
> `False`.  [**Question**: there cannot be a function type that is
> analagous to the axiom that deals with `False`: why?]
>
> Different combinations of `const` and `subst` correspond to proofs
> of propositions that do not mention `False`.  As an illustration,
> work out what `mystery` represents:
```haskell
mystery :: LExp Char
mystery = (App (App substL constL) constL)
```
> Hint: β-reduce `mystery` until it can be reduced no further.

Programming in the lambda-calculus is a bit like programming with
logic gates: everything needs to be encoded from the ground up.

As an example, we encode natural numbers as Lambda Expressions; these
are called "Church numerals", after their inventor, Alonzo Church.
The idea is that the numeral `n` is represented by a function that has
two parameters, _function_ and _data_, and applies _function_ `n`
times to _data_.  The structure directly reflects the Peano axioms for
the natural numbers, which describes the relationships between a
representation of zero, and a representation of the successor, or
`(+1)` function.
```haskell
zeroL, succL :: LExp Char
zeroL = Lambda 'f' (Lambda 'x' (Name 'x'))
succL = Lambda 'n' (Lambda 'f' (Lambda 'x' (App (Name 'f') (App (App (Name 'n') (Name 'f')) (Name 'x')))))

oneL, twoL, threeL :: LExp Char
oneL   = App succL zeroL
twoL   = App succL oneL
threeL = App succL twoL
```
We could have used the functions we have already defined to define
`zeroL`, here is an alternative definition:
```haskell
zeroL' :: LExp Char
zeroL' = App constL idL 
```
As a final example we will encode Booleans.

A Boolean is encoded by a function of two arguments.  If the Boolean
is:
* **true** then the _second_ argument is returned
* **false** then the _first_ argument is returned

We will define one value and one operator; all the others can be
constructed from these two. (Compare with the construction of the
Natural numbers from a value, _zero_ and one function, _successor_.)
The two are encodings of `false` and `implication`.  Again we can use
a function we have already defined to construct `false`:
```haskell
falseL, impliesL :: LExp Char
falseL = constL
impliesL = Lambda 'b' (Lambda 'c'
             (Lambda 'x' (Lambda 'y'
                (App (App (App substL (Name 'b'))
                      (App (Name 'c') (Name 'x'))) (Name 'y')))))

```
Now we can encode the other operators and `true`:
```haskell
notL, trueL, orL, andL :: LExp Char
notL = Lambda 'b' (App (App impliesL (Name 'b')) falseL)
trueL = App notL falseL
orL = Lambda 'b' (Lambda 'c'
                  (App (App impliesL
                        (App notL (Name 'b')))
                        (Name 'c'))) 
andL = Lambda 'b' (Lambda 'c'
                   (App notL (App (App orL
                                   (App notL (Name 'b')))
                                   (App notL (Name 'c')))))
```
## Sequel

It tuns out that the Lambda calculus allows proofs of false.  This
problem has cropped up in many other attempts to provide a simple
underpinning to mathematics (see, for example, Russell's paradox that
broke a naïve attempt to build set theory).  An almost universal
solution is to add something akin to a type system.  A typed version
of the Lambda Calculus is what actually underlies Haskell; this
version is known as _System F_.

## References

[1]: <https://doi.org/10.1145%2F359576.359579> Backus, J. (1978). "Can
     programming be liberated from the von Neumann style?: A
     functional style and its algebra of programs". Communications of
     the ACM. 21 (8): 613–641

[2]: <https://youtu.be/IOiZatlZtGU> Philip Wadler, "Propositions as
Types", Strange Loop, St Louis, 2015.
```haskell

