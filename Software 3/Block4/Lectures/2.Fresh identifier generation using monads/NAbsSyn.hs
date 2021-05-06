module NAbsSyn where

{-
Copied from the lecture _TypeSys_.

These data structures define an abstract syntax for a tiny imperative
language called N.
-}

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
