module NUtil where

{-
# Extended example of recursion
# Jeremy Jacob

In this file we provide examples of recursive computation over
recursive data structures.  In particular, recursion over the trees
that represent programs in the trivial imperative language, _N_.

The abstract syntax of the language is represented in the types
defined in the file `NAbsSyn`, so we import it, in full.
-}
import NAbsSyn
{-
We also import [a utility to work with
sets](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Set.html)
that is used for checking initialisation before use.  (All imports
must occur immediately following the `module` statement, so we cannot
delay this until needed.)
-}
import Data.Set
{-
## Pretty printing

Our first utility is one to pretty print an abstract syntax tree in a
plain text form.  In fact, all this function does is to convert a
value in `N` into a string that is suitable for printing.  There are
two ways to achieve this, given `p::N`.
1. Call `putStrLn (prettyprint p)`.
2. Instantiate `Show N` with `show = prettyprint`, and then call
   `print p`.

The function is a kind of generalisation of a right-fold, building the
value at a node by combining it with the result of processing inferior
structures.

### Top level: N programs
An N program is a tagged statement, so we need to pretty print
statements.  That is handled by `ppStmt`.
-}
prettyprint :: N -> String
prettyprint (N s) = ppStmt s
{-
### Layout utilities
Pretty printing involves ensuring that indentation of statements is
correct.  This is handled by the utility `pad`, which computes a
string consisting of a newline followed by a number of spaces.  We
also introduce a standard tab distance.
-}
pad :: Int -> String -- n spaces
pad    n    = '\n' : replicate n ' '

tab :: Int
tab  = 2
{-
### Pretty printing statements

Elements of a statement might be
* base cases (for example, the null statement), which are handled
  directly,
* types (in block statements), which are a kind of base case handled
  by a call to a function to pretty print types.
* expressions (in, for example, the print and assignment statements),
  which is a special kind of base case, involving a call to a further
  function to pretty print expressions, or
* statements (in, for example, sequential composition and loops),
  which entail a **recursive** call of the pretty printer, perhaps
  with an increased level of indentation.

-}
ppStmt :: Stmt -> String
ppStmt  = (flip (++) "\n") . tail . pp 0
  where
    pp n Skip = pad n ++ "Skip"
    pp n (Print e) = pad n ++ "PRINT " ++ ppE e
    pp n (v := e) = pad n ++ v ++ " := " ++ ppE e
    pp n (p :> q) = pp n p ++ ";" ++ pp n q
    pp n (Ifte e p q) =    padn   ++ "IF"
                        ++ pad nt ++   ppE e
                        ++ padn   ++ "THEN"
                        ++ ppnt        p
                        ++ padn   ++ "ELSE"
                        ++ ppnt        q
                        ++ padn   ++ "FI"
      where padn  = pad n
            nt    = n + tab 
            ppnt  = pp nt
    pp n (While e p) =    padn ++ "WHILE"
                       ++ pad nt ++  ppE e
                       ++ padn ++ "DO"
                       ++            pp nt p
                       ++ padn ++ "OD"
      where padn = pad n
            nt = n + tab
    pp n (Block v t p) =    padn ++ "BEGIN"
                         ++ pad nt ++ v ++ " : " ++ ppT t
                         ++ padn ++ "IN" ++ pp nt p
                         ++ padn ++ "END"
      where padn = pad n
            nt   = n + tab
{-
### Types

_N_ has a very simple structure, and every case is a base case (the
function is not recursive).
-}
ppT :: Type    -> String
ppT    IntType  = "Integer"
ppT    BoolType = "Boolean"

{-
### Expressions

Expressions are as complicated to pretty print as statements.  An
expression may have elements that are:
* Base cases (for example, Boolean constants).
* Further expressions (for example, in addition and implication).
  Apart from arithmetic negation, all subexpressions are embedded in
  binary operators, so we introduce a utility to handle binary
  operators, `binop`.

For simplicity the pretty printed expressions are fully bracketed.
There is no attempt to remove brackets using standard conventions on
operator priority.
-}
binop :: Expr -> Expr -> String -> String
binop    a       b       s       = "(" ++ ppE a
                                   ++ " " ++ s ++ " "
                                   ++ ppE b ++ ")"

ppE :: Expr      -> String
ppE    (Ref v)    = v
ppE    (ValInt i) = show i
ppE    ValTru     = "TRUE"
ppE    ValFls     = "FALSE"
ppE    (a :+: b)  = binop a b "+"
ppE    (a :*: b)  = binop a b "*"
ppE    (Neg a)    = "(-" ++ ppE a ++ ")"
ppE    (a :=>: b) = binop a b "=>"
ppE    (a :<: b)  = binop a b "<"
ppE    (a :=: b)  = binop a b "="

{-
## Type checking

Another important recursive function is typechecking.  In _N_ this is
about as primitive as it can get, given that there are only two types:
Integers and Booleans.  Real languages have far more sophisticated
type systems, and so are much more complex to check.  The Haskell type
system has another level of complexity, in that it is able to deduce
types as well as check types, and has to cope with user-defined
types.

Again, we can see this function as a generalisation of a right fold.

First we introduce some utilities to report errors.  Error reports are
strings (which can be delivered to the user through calls to
`putStr`).  Error messages are concatenated together, to produce a
list of such message.  In that case, "no error" is best represented by
the unit of concatentation, `""`.
-}
no_error :: String
no_error  = ""

checktype :: Type -> Type -> String
checktype    exp     fnd -- expected/found
  | exp == fnd = no_error
  | otherwise  = "Type "++show exp++" expected, but type "++show fnd++" found\n"

{-
To check a variable we introduce a type to represent the environment,
which keeps all the currently defined variable names, together with their
types.  We take advantage of `Prelude` functions such as [`lookup ::
Eq a => a -> [(a, b)] -> Maybe
b`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:lookup).
-}
type Env = [(Var, Type)]

checkvar :: Env -> Var -> (Type -> String) -> String 
checkvar    g      v         f
  = case lookup v g of
      Nothing -> "No such variable: `" ++ v ++ "'\n"
      Just s  -> f s

{-
Now we can write the typechecking functions, in a similar structure to
pretty printing.
-}

typecheck :: N  -> String
typecheck (N source) = typecheckStmt [] source

typecheckStmt :: Env -> Stmt -> String
typecheckStmt    g    = tcs
  where
    tcs Skip          = no_error
    tcs (Print e)     = typecheckExpr g IntType e
    tcs (v := e)      = checkvar g v (\s -> typecheckExpr g s e)
    tcs (p :> q)      = tcs p ++ tcs q
    tcs (Ifte e p q)  = typecheckExpr g BoolType e ++ tcs p ++ tcs q
    tcs (While e p)   = typecheckExpr g BoolType e ++ tcs p
    tcs (Block v t s) = typecheckStmt ((v,t):g) s

typecheckExpr :: Env -> Type -> Expr -> String
typecheckExpr    g      t     = tce
  where
    tce (Ref v)    = checkvar g v (checktype t)
    tce (ValInt _) = checktype t IntType
    tce ValTru     = checktype t BoolType
    tce ValFls     = checktype t BoolType
    tce (a :+: b)  = checktype t IntType ++ tcei a ++ tcei b
      where tcei = typecheckExpr g IntType
    tce (a :*: b)  = checktype t IntType ++ tcei a ++ tcei b
      where tcei = typecheckExpr g IntType
    tce (Neg e)    = checktype t IntType ++ typecheckExpr g IntType e
    tce (a :=>: b) = checktype t BoolType ++ tceb a ++ tceb b
      where tceb = typecheckExpr g BoolType
    tce (a :<: b)  = checktype t BoolType ++ tcei a ++ tcei b
      where tcei = typecheckExpr g IntType
    tce (a :=: b)  = checktype t BoolType ++ tcei a ++ tcei b
      where tcei = typecheckExpr g IntType

{-
## Checking initialisation before use.

Another generalisation of right folding is to check if every use of a
variable is of an initialised variable.  In this we use
[`Data.Set`](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-Set.html)
to keep the set of variables currently initialised.  Functions exported by this library are:
* `empty :: Set a`, the empty set
* `insert :: Ord a => a -> Set a -> Set a`, to insert an element in a
  set, and
* `member :: Ord a => a -> Set a -> Bool`

In this case we have opted to make the functions that process
statements and expressions as functions local to the main function.
This is arguably better software engineering, but it does mean that it
is difficult to test each component separately.  An alternative is to
explicitely export only the user-level functions from the module.
-}
checkInitB4Use :: N -> String
checkInitB4Use (N s) = ci empty s
  where
    ci _ Skip            = no_error
    ci s (Print e)       = cv s e
    ci s (v := e)        = cv s e
    ci s ((v := e) :> p) = cv s e ++ ci (insert v s) p -- an assignment records variable as initialised
    ci s (p :> q)        = ci s p ++ ci s q
    ci s (Ifte e p q)    = cv s e ++ ci s p ++ ci s q
    ci s (While e p)     = cv s e ++ ci s p
    ci s (Block v _ p)   = ci (delete v s) p -- v is no longer initialised
    
    cv _ (ValInt _) = no_error
    cv _ ValTru     = no_error
    cv _ ValFls     = no_error
    cv s (Ref v)    | v `member` s = no_error
                    | otherwise    = "Variable `" ++ v ++ "' not initialised\n"
    cv s (a :+: b)  = cv s a ++ cv s b
    cv s (a :*: b)  = cv s a ++ cv s b
    cv s (Neg a)    = cv s a
    cv s (a :=>: b) = cv s a ++ cv s b
    cv s (a :<: b)  = cv s a ++ cv s b
    cv s (a :=: b)  = cv s a ++ cv s b

{-
## Simplifying programs

The first step in optimisation is to simplify a program before compilation.

For example, an assignment such as:
```haskell
x := (ValInt 3 :+: ValInt 2)
```
should be simplified to
```haskell
x := ValInt 5
```

This program is also a generalisation of right folding, however it is
more subtle than the earlier utilities.  In none of the earlier
utilities did the value at a node depend on the values computed below.
In simplification we must ensure that we know the simplifications of
the subtrees before deciding on the simplification at the node.

The expression
```haskell
ValInt 1 :+: (ValInt 0 :+: ValInt 0)
```
ought to simplify to `ValInt 1`, but the naive algorithm simplifies it
to `ValInt 1 :+: ValInt 0` because when it looks at the top level it
does not know to what the right-hand argument will simplify.

We can achieve the effect of delaying the computation at the node by
introducing for each node a function that replaces the constructor,
sometimes called a "smart constructor".  This function can examine its
arguments in ways which dumb constructors cannot.  In cases where the
smart constructors can do nothing else, they fall back on the dumb
constructor.
-}
simplify :: N   -> N
simplify   (N s) = N (simplifyStmt s)

simplifyStmt :: Stmt     -> Stmt
-- Note we can leave all cases of "no change" to a single clause at the end
-- rules for print
simplifyStmt    (Print e) = print (simplifyExpr e)
  where -- smart constructor for print (rather dumb!)
    print e = Print e
-- rules for assignment
simplifyStmt    (v := e)  = v `ass` (simplifyExpr e)
  where -- smart constructor for :=, one smart cases
    v' `ass` e'@(Ref w) | v==w      = Skip
                        | otherwise = v `ass` e'
    v' `ass` e'         = v' := e'
-- rules for sequential composition
simplifyStmt (p :> q)   = simplifyStmt p `seq` simplifyStmt q
  where -- two smart cases, to remove redundant Skips
    Skip `seq` q    = q -- Left unit
    p    `seq` Skip = p -- Right unit
    p    `seq` q    = p :> q
-- rules for binary selection
simplifyStmt (Ifte e p q)   = ifte (simplifyExpr e) (simplifyStmt p) (simplifyStmt q)
  where -- three smart cases
    ifte ValTru p _ = p
    ifte ValFls _ q = q
    ifte e      p q | p==q      = p
                    | otherwise = Ifte e p q
-- rules for iteration
simplifyStmt (While e p) = while (simplifyExpr e) (simplifyStmt p)
  where -- one smart case
    while ValFls _ = Skip
    while e      p = While e p
-- rules for blocks
simplifyStmt (Block v t p) = block v t (simplifyStmt p)
  where -- one smart case
    block _ _ Skip = Skip
        -- other possible smart rules:
        --   if v is not free in p, replace Block v t p by p
        --   if v is only free variable in p, replace Block v t p by Skip
        --        (Skip rule is a special case of this rule) 
    block v t p    = Block v t p
-- all operators others: no change
-- [Just Skip; but if we add new operators to the abstract syntax of statements the function would still work,
--  but not simplify the argument(s) of the new operator.]
simplifyStmt    e          = e

simplifyExpr :: Expr -> Expr
-- rules for summation
simplifyExpr (a :+: b) = simplifyExpr a `sum` simplifyExpr b
  where -- three smart cases
    ValInt m `sum` ValInt n = ValInt (m+n)
    a        `sum` ValInt 0 = a -- Right unit of addition
    ValInt 0 `sum` b        = b -- Left unit of addition
    a        `sum` b        = a :+: b 
-- rules for multiplication
simplifyExpr (a :*: b) = simplifyExpr a `mul` simplifyExpr b
  where -- five smart cases
    ValInt m `mul` ValInt n = ValInt (m*n)
    _        `mul` ValInt 0 = ValInt 0 -- Right zero of multiplication
    ValInt 0 `mul` _        = ValInt 0 -- Left zero of multiplication
    a        `mul` ValInt 1 = a        -- Right unit of multiplication
    ValInt 1 `mul` b        = b        -- Left unit of multiplication
    a        `mul` b        = a :*: b
-- rules for negation
simplifyExpr (Neg e)            = neg (simplifyExpr e)
  where -- two smart cases
    neg (ValInt n)          = ValInt (negate n)
    neg (Neg e)             = e -- self-inverse
    neg e                   = Neg e
-- rules for implication
simplifyExpr (a :=>: b) = simplifyExpr a `impl` simplifyExpr b
  where -- four smart cases
    ValTru `impl` ValFls          = ValFls
    ValFls `impl` _               = ValTru
    _      `impl` ValTru          = ValTru
    (e :=>: ValFls) `impl` ValFls = e -- double negation
    a      `impl` b               = a :=>: b
simplifyExpr (a :<: b) = simplifyExpr a `lt` simplifyExpr b
  where -- two smart cases
    ValInt m `lt` ValInt n | m < n     = ValTru
                           | otherwise = ValFls
    a `lt` b               = a :<: b
simplifyExpr (a :=: b) = simplifyExpr a `eq` simplifyExpr b
  where -- two smart cases
    ValInt m `eq` ValInt n | m == n    = ValTru
                           | otherwise = ValFls
    a `eq` b               = a :=: b
-- all others: no change
-- [At the moment: just variables and constants;
--  if we added new operators to the abstract syntax of expressions
--  the function would still work,
--  but not simplify the operator's argument(s).]
simplifyExpr e = e
