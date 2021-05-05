```haskell
module FoldOps where
```

# Kofi Appiah, 10 Jan 2021 rev 29/1/21

# Fold Operations 

### Basics



List comprehension is actually syntactic sugar for a combination of `map` and `filter`.  So map and filter can be expressed using list comprehension. For example the `map` function can be defined recursively as `recMap`:

```haskell
recMap :: (a -> b) -> [a] -> [b]
recMap op [] = []
recMap op (x:xs) = op x : recMap op xs
```

And defined using list comprehension  as `map'`:

```haskell
map' op xs = [op x | x <- xs]
```

`map` is a special case of `foldr`: `map f = foldr ((:) . f) []`. `foldr` captures the pattern in `[a, b, c] |-> a+(b+(c+0))`. *Folding* is a concept that extends in usefulness and importance beyond lists, and most primitive recursive functions over lists can, in fact, be defined using a fold. To emphasise on this, _**folding**_ is used to capture a common pattern of recursion. In contrast to list operations like `map`, `filter` and `reverse`, which iterate over a list and produce a list, *fold* operations iterate over list by combining all the elements with some operator. Because a list can possibly be traversed is in two different directions, there are two versions of the fold operation: **foldr** and **foldl**. `foldr` is the fold from right function and `foldl` is the fold from the left function. The fold functions take three arguments, the first is the **operator** to be used to combine the elements of the list, the second is a constant or **starting value** (needed to ensure the fold can produce a result when applied to an empty list) and the third argument is the **list** over which the iteration takes place. With the three arguments, a fold operation can be implemented by iterating over a list with the *starting value* acting as an *accumulator* that gives the current intermediate results. There is also the function `fold`, which does not need the operator or constant parameters as long as the embedded type is a `Monoid`: it right-folds using the monoid operator and identity.  Like `foldr` and `foldl`, it works for any type constructor `t` which is `Foldable` (which will be covered in Block 3 under type classes), and not just lists.

This topic will:

1. explore the two fold operations (*partly covered in Block 1 problems but explicitly re-enforced here*),
2. provide a detail evaluation of the two fold operations and
3. describe some functions with folding. 

#### Right-folding : *foldr*

Recall that a list such as `[5, 7, 6, 2]` is actually syntactic sugar for `5:(7:(6:(2:[])))`.  A right fold replaces the list-constructor, "cons", `(:)` by an operator and the empty list, `[]`, by a constant or the starting value. The type of `foldr` which can easily be understood based on what is covered so far is `(a -> b -> b) -> b -> [a] -> b` in **GHC 7.8** and older. This has since changed to `Foldable t =>  (a -> b -> b) -> b -> t a -> b` and will be discussed under *Type Classes* in Block 3. For now, just assume the only change is from the list `[]` in `[] a` into a more generic typeclass `t` in `t a`.  It is called a right-fold because the expression associates to the right; thus the parentheses are grouped to the right and the starting value or accumulator comes in at the right.  The function `foldr` can be defined recursively as:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op unit []     = unit
foldr op unit (x:xs) = x `op` (foldr op unit xs)
```

Looking at the recursive definition of the `map` function `recMap`, there is a noticeable parallel between `recMap` and `foldr`. `recMap` applies a function `op` to each element of the list and returns a list; a `foldr` replaces the cons `:` operator with the function `op`, replace `[]` with the starting value `0` and reduces the list. For example, `recMap (+1) [5, 7, 6]` is `(+1) 5 : (+1) 7 : (+1) 6 : []` and `foldr (+) 0 [5, 7, 6]` is `5 + (7 + (6 + 0))`.  

The actual parallel between `map` and `foldr` is demonstrated with:

```haskell
map (+1) [5, 7, 6]
== let n +: ns = (n+1) : ns in foldr (+:) [] [5, 7, 6]
== let n +: ns = (n+1) : ns in 5 +: (7 +: (6 +: []))
== let n +: ns = (n+1) : ns in 5 +: (7 +: [7])
== let n +: ns = (n+1) : ns in 5 +: [8, 7]
== let n +: ns = (n+1) : ns in [6, 8, 7]
== [6, 8, 7]
```

In general given an operator `(+)`, a starting value `x` and a list `[a, b, c]` the application `foldr (+) x [a, b, c]` result in a single value computed by a sequence of calculations using the `(+)` operator:

```haskell
foldr (+) x [a, b, c]
~~>
(a + (b + (c + x)))
```

Recall the recursive definition of the function `reverse` as:  

```haskell
recRev :: [a]    -> [a]
recRev    []     = []
recRev    (x:xs) = recRev xs ++ [x]
```

`foldr` is used to define the `reverse` function as:

```haskell
revRF :: [a] -> [a]
revRF = foldr (\ x xs -> xs ++ [x]) []
```

The starting value in `revRF` is `[]` and the operator is the lambda expression `\ x xs -> xs ++ [x])`, which takes an element and a list and concatenate the element to the end of the list. For example `revRF [2, 3, 4]` is evaluated as:

```haskell
(\ x xs -> xs ++ [x]) 2
    ((\ x xs -> xs ++ [x]) 3
        ((\ x xs -> xs ++ [x]) 4 []))
```

Similarly, the recursive definition of the function `length` :

```haskell
recLen :: [a] -> Integer
recLen    []   = 0
recLen    (_:xs) = 1 + recLen xs
```

can be defined with `foldr` as:

```haskell
lenRF :: [a] -> Integer 
lenRF = foldr (const (1+))        0
```

It is worth noting that `const` in the definition of `lenRF` is doing the same job as the `_` in `recLen`: both are saying that the actual value in the list is irrelevant, only its presence is important. Thus in the definition of `lenRF`, the starting value is `0` and the operator is `(const (1+))`. For example `lenRF [2, 3, 4]` is evaluated as:

```haskell
(const (1+)) 2
   ((const (1+)) 3
      ((const (1+)) 4 0))
~~>
(const (1+)) 2
   ((const (1+)) 3 1)
~~>
(const (1+)) 2 2
~~>
3
```

As a final example, based on the definition of right-folding from Block 1 exercise Q6.4; thus "a right fold replaces the list-constructor, "cons", `(:)` by an operator and the empty list, `[]`, by a constant". The defined insertion function using a right fold `insert'` in Block 1 Q6.6, can be expanded in a similar way.

```haskell
insert' :: Ord a => a -> [a] -> [a]
insert' x = foldr insx [x]
  where
    insx y ys@(z:zs) | z==x && x<y = z:y:zs
                     | otherwise   = y:ys
```

From the definition of `insert'`, using `foldr insx [x]`, the operator is `insx` and the constant or initial value is `[x]`. Using `insert' 6 [5, 7, 8]` as an example, `foldr` will be applied to `insx [6]` and if `(:)` is replaced by `insx` and the `[]` is replaced by `[6]` in `5:(7:(8:[]))` then it will be evaluated in the following sequence:

```haskell
5 `insx` (7 `insx`(8 `insx` [6]))
5 `insx` (7 `insx`(insx 8 [6])) -- 6==6 && 6<8 = 6:8:[]
5 `insx` (insx 7 6:8:[])        -- 6==6 && 6<7 = 6:7:8:[]
insx 5 (6:7:8:[])                 -- otherwise = 5:6:7:8:[]
```

Hence the returned list would be `[5, 6, 7, 8]`.

#### Left-folding : *foldl*

Recall that a list such as `[5, 7, 6, 2]` is actually syntactic sugar for `5:(7:(6:(2:[])))`.  A left fold applies the operator on the constant or starting value and the first element of the list. The returned new accumulator value is used with another element of the list recursively as arguments to the operator. The type of `foldl` which again can easily be understood base on what is covered so far is `(a -> b -> a) -> a -> [b] -> a` in **GHC 7.8** and older. This has also changed to `Foldable t =>  (a -> b -> a) -> a -> t b -> b`. For now, just assume the only change is from the list `[]` in `[] b` into a more generic typeclass `t` in `t b`.  It is called a left-fold because the expression associates to the left; thus the parentheses are grouped to the left and the starting value or accumulator comes in at the left.  The function `foldl` can be defined recursively as:

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl op unit []     = unit
foldl op unit (x:xs) = foldl op (op unit x) xs
```

For example `foldl (-) 0 [4, 6, 8]` is evaluated as:

```haskell
foldl (-) 0 [4, 6, 8]
~~>
(-)((-) ((-) 0 4) 6) 8
~~>
(-)((-) (-4) 6) 8
~~>
(-) (-10) 8
~~>
-18
```

In general given an operator `(+)`, a starting value `x` and a list `[a, b, c]` the application `foldl (+) x [a, b, c]` result in a single value computed by a sequence of calculations using the `(+)` operator:

```haskell
foldl (+) x [a, b, c]
~~>
(((x + a) + b) + c)
```

An insight into `foldl` for imperative programmers. Consider the imperative code pattern:

```
accumulator := initial
current := POINTER TO linkedlist
WHILE current /= NULL THEN
  accumulator := update(head(current), accumulator)
  current := tail(current)
```

The only things of interest are:

- the values of `update`, `initial`, and `linkedlist` , and
- the overall shape, which is exactly `foldl`!

It is harder to do the same thing for foldr, because foldr exploits laziness, and any imperative code would need to explicitly implement the laziness. To see the difference, consider, for example:

```
ones = 1 : ones -- an infinite list
ok = head (foldr ((:) . (*2)) [] ones) -- head (map (*2) ones)
ko = head (foldl (flip ((:) . (*2))) [] ones)
```

`ok` returns a value, but `ko` runs forever.

Recall from the "Block 1 problems" that sometimes a right fold is less efficient than a left fold.  The left fold of `(+)` and `0` over `[5, 7, 6, 2]` is `(((0+5)+7)+6)+2`.  When a homogenous (that is, has type `a -> a -> a`) operator is _associative_, such as `(+)`, `(*)`, and `(++)`, the left and right folds are equal.  When the operator is not associative, such as `(-)`, the two folds may give different answers.  When the operator is not homogenous (that is, it has type `a -> b -> b` for _different_ `a` and `b`), the left fold of `flip f` may be different to the right fold of `f`. Compared to right fold, foldl will fail on an infinite list and for some operators like `map`, foldr can produce partial results on infinite lists. 

### Functional Composition 

The composition operator `(.)` is used to create a pipeline of function applications, each of which would be waiting for an argument. It is considered as a higher-order library operator for composing two functions into a single function, such that the results of applying one function gets passed to the next function as an argument. It is a very concise style, in keeping with the terse functional style Haskell is known for. The composition operator can be defined as:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

Which can be read as given a function `b` to `c`, and a function `a` to `b`, return a function `a` to `c`. That is, `f . g`, which is read as `f` *composed with* `g`, (or from THE1, `f o g` read as `f` after `g`), is the function that takes an argument `x`, applies the function `g` to this argument, and applies the function `f` to the result. The operator could also be defined by `(f . g) x = f (g x)`. Composition is used to simplify nested function applications, reduce the number of parentheses needed and provide implicit definitions by avoiding reference to initial arguments. For example the in Block 1 Q5.5, function composition was used in the definition of `implies_` with implicit parameters. 

```haskell
implies_ :: Bool -> Bool -> Bool 
implies_ = (||) . not 
```

Where `(||)` is the function `f` and `not` the function `g`, then `implies_` applied to `True False` will evaluate as:

```haskell
implies_ True False
~~>
f . g True False
~~>
f(g True) False
~~>
f(not True) False
~~>
f (False) False
~~>
(||) False False
~~>
False
```

Thus the first (rightmost) function `not` receives its arguments, then gives its results to the function on the left `(||)`.  The function `(||)` returns the final result of the application. 

For example this function application `((:) . toUpper) 's' "of3"` evaluates to `Sof3`. The first function `toUpper` receives the `'s'` and return `'S'`, then the *cons* function receives the `'S'` and the string, and creates the new string `"Sof3"`. **Note** to test this you need to "import Data.Char" to apply `toUpper`. It is possible and also easier to compose more than two functions using function composition. Example `take 3 . filter even. map (*2) $ [1..10]` will evaluate to `[2, 4, 6]`. As more functions are composed it becomes clear that nesting all the parentheses can be tiresome and function composition avoids doing that. It allows *pointfree* definition of functions, thus composing functions without specifying their arguments as in the implicit description of `implies_` in Q5.5 from Block 1. 

