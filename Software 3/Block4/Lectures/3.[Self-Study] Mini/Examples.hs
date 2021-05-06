module Examples where
import N.AbstractSyntax


ppN :: N -> IO ()
ppN = putStrLn . prettyprint

-- The problem with naive simplification is exposed by this program: 
simplifyProblemE :: Expr
simplifyProblemE =  (ValInt 3 :+: (Neg $ ValInt 3)) :*: (Ref "x" :+: ValInt 4)
simplifyProblemS :: N
simplifyProblemS = N (Block "x" IntType
                      (  "x" := ValInt 1
                        :> (Ifte (ValFls :=>: ValFls)
                             ("x" := simplifyProblemE)
                             Skip)
                        :> (While ValFls (Print (Ref "x")))
                        :> "x" := ValInt 99
                        :> Print (Ref "x")))

-- Example with both an initialised variable ("x")
-- and an unitialised variable ("b")
initProblemS :: N
initProblemS =
  N (Block "x" IntType
     (  ("x" := ValInt 5)
       :> (Block "b" BoolType
           (Ifte (Ref "b")
             Skip
             (Print ((Ref "x" :+: ValInt 0) :*: ValInt 2))))
       :> Print (Ref "x")))
    

-- sum the numbers 1..n
sumUptoN :: Int -> N -- a parametrised macro
sumUptoN n =
  N (Block "sum" IntType
     (   "sum" := ValInt 0
       :> (Block "i" IntType
           (   "i" := ValInt 0
             :> (While (Ref "i" :<: (ValInt n :+: ValInt 1))
                  ("sum" := Ref "sum" :+: Ref "i" :> "i" := Ref "i" :+: ValInt 1))
             :> Print (ValInt n)
             :> Print (Ref "sum")
             :> Print (ValInt n :*: (ValInt n :+: ValInt 1))))))

-- test simplification of multiple negations
multipleNeg :: N
multipleNeg =
  N (Block "b" BoolType
     ("b" := ValTru
      :> test id                   0
      :> test notE                 1
      :> test (notE . notE)        2 
      :> test (notE . notE . notE) 3
     ))
  where -- these definitions play the role of macros
    notE     = (:=>: ValFls)
    prn      = Print . ValInt
    prnn     = prn . negate
    test f n = Ifte (f (Ref "b")) (prn n) (prnn n)

-- Hole-in-scope examples
holeInScopeA :: N
holeInScopeA =
  N (Block "v" IntType
      ("v" := ValInt 0
       :> (Block "v" IntType
            ("v" := ValInt 1
             :> (Block "v" IntType
                  ("v" := ValInt 2
                   :> Print (Ref "v")))
             :> Print (Ref "v")))
       :> Print (Ref "v")))
  

holeInScopeB :: N
holeInScopeB =
  N (Block "v" IntType
      ("v" := ValInt 0
       :> (Block "w" IntType
            ("v" := ValInt 1
             :> (Block "v" IntType
                  ("v" := ValInt 2
                   :> Print (Ref "v")))
             :> Print (Ref "v")))
       :> Print (Ref "v")))

-- Choice

chooseA, chooseB :: N
chooseA =
  N (Block "b" BoolType
      ("b" := ValInt 1 :=: ValInt 2
       :> Ifte (Ref "b") (Print (ValInt 8)) (Print (ValInt 9))
      ))
chooseB =
  N (Block "b" BoolType
      ("b" := ValInt 1 :=: ValInt 1
       :> Ifte (Ref "b") (Print (ValInt 8)) (Print (ValInt 9))
      ))
