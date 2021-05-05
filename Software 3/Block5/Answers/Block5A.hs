module Block5Problems where
import Control.Exception
import System.IO
import System.IO.Error
import Test.QuickCheck
import Data.List

{-
# SOF3: Block 5 Problems

## Question: Exception handling

In the lecture on Exception handling we gave code similar to:
-}
filterFile ::    (String -> Bool)   -- to select lines
              -> FilePath           -- source file
              -> FilePath           -- destination file
              -> IO ()
filterFile select source destination =
  withFile source ReadMode
    (\ s -> withFile destination WriteMode
              (\ d -> forever (transferOneLine select s d)))
               -- forever terminates when an EOF exception is thrown in transferOneLine
  where
    forever act = let fra = do {act; fra} in fra
    
transferOneLine :: (String -> Bool) -> Handle -> Handle -> IO ()
transferOneLine p s d = do
  line <- hGetLine s
  if p line
    then hPutStrLn d line
    else pure () -- do nothing

{-
We gave an example of the use of `filterFile` with the predicate
`isTitle`:
-}
isTitle :: String -> Bool
isTitle ('#':_) = True
isTitle _       = False
{-
to select title lines from a Markdown format file.

The function `filterFile` has the unpleasant behaviours that:
1. it terminates with an exception, even in the case where the exception
   arises because of normal behaviour: end-of-file is detected; and
2. it breaks out of the loop because of exceptional behaviour outside the
   code, rather than because of a controlled test within the code.

We will explore each of the problems separately.

### Fixing Problem 1
You should create a function `filterFile2` that passes on truly
unexpected exceptions but not end-of-file exceptions.  To do this you
will need the function `catchJust` from module
[`Control.Exception`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html)
and `isEOFError` from module
[`System.IO.Error`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Error.html)
to put a wrapper around `filterFile`.  Note that you will need to put
a wrapper around `isEOFError` so that the answer is a `Maybe` value,
rather than a `Bool` value.
-}
filterFile2 ::    (String -> Bool)   -- to select lines
               -> FilePath           -- source file
               -> FilePath           -- destination file
               -> IO ()
filterFile2 select source dest = catchJust isEOF body handle
  where
    isEOF e | isEOFError e = Just () -- value of e not needed after the test
            | otherwise    = Nothing
    body = filterFile select source dest
    handle _ = putStrLn "Transfer complete"                       
{-
### Fixing Problem 2
To fix Problem 2 you will need to amend the body of `filterFile` so
that the end-of-file is detected _before_ the exception is raised.  To
do this you will need to replace the `forever` block with one that
tests for end-of-file before executing the body.  The simplest way to
do this is to replace `forever (transferOneLine s d)` with a recursive
function.  You will need `hIsEOF` from module
[`System.IO`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html).
-}
filterFile3 ::    (String -> Bool)   -- to select lines
               -> FilePath           -- source file
               -> FilePath           -- destination file
               -> IO ()
filterFile3 select source destination =
    withFile source ReadMode
             (\ s -> withFile destination WriteMode
                              (\ d -> body s d (transferOneLine select)))
  where
    body s d act = bsda
      where
        bsda = do
          eof <- hIsEOF s
          if eof
            then putStrLn "Transfer complete"
            else do {act s d; bsda}
{-
## Question: Strictness

Define a strict version of `foldr`, `foldr'`.  When is it appropriate
to use `foldr'`?
-}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z = frfz
  where
    frfz []     = z
    frfz (x:xs) = f x $! frfz xs
{-
It only make sense to use this if `f` is strict in its second argument,
for example: `(+)`, `\ xs ys -> reverse ys ++ xs`.
-}

{-
## Question: Testing

### Problem 1
Given a list of students, where a student has type: `newtype Student = Student [(Module, Maybe Int)]` -- or `newtype Student m = Student [(m, Maybe Int)]` check that  where a mark has been allocated (ie `Just n` rather than `Nothing`, it is in the range 0-100).
-}

newtype Student = Student [(Module, Maybe Int)] deriving Show
data Module = THE1 | SOF1 | THE2 | SOF2 | SYS1 | DAT1 | HCI1 deriving (Show, Eq)

checkMarks :: [Student] -> Bool
checkMarks ss = and [and [0 <= m && m <= 100 | (_, Just m) <- s] | (Student s) <- ss]

csStage, csStage1, csStage2 :: [Student]
csStage = [Student [(SOF1, Just 140), (THE2, Nothing)], Student [(SOF1, Nothing), (SOF2, Nothing)], Student [(SOF1, Just 56)]]
csStage1 = [Student [(SOF1, Just 40), (THE1, Nothing)], Student [(SOF1, Nothing), (THE1, Nothing)], Student [(SOF1, Just 56), (THE1, Just 77)]]
csStage2 = [Student [(SOF1, Just 40), (SOF2, Nothing)], Student [ (THE1, Just 5), (SOF1, Nothing), (SOF2, Nothing)], Student [(SOF1, Just 56), (HCI1, Just 47)]]


{-
### Problem 2
Given a function `prereq :: Module -> [Module]` check that a student is not taking modules for which they do not have the prerequisites.
-}

prereq :: Module -> [Module]
prereq THE2 = [THE1, SOF1]
prereq SOF2 = [THE1, SOF1]
prereq SYS1 = [SOF1]
prereq DAT1 = [THE1]
prereq _    = []

checkPrereqs :: Student -> Bool
checkPrereqs (Student ms) = and [and [p `elem` mods | p <- prereq m] | m<-mods]
  where mods = [m | (m, _) <- ms]


{-
### Problem 3 - QuickCheck 
For a function `quarterId`, write a suitable property `prop_quarter` and use QuickCheck to verify that `quarterId` holds.
-}

quarter :: (Fractional a) => a -> a
quarter x = x / 4

quarterId :: (Fractional a) => a -> a
quarterId = (*4) . quarter

prop_quarter :: Float ->  Bool
prop_quarter x = quarterId x == id x


{-
### Problem 4 - QuickCheck 
For a function `listOrdered`, write a suitable property and use QuickCheck to verify that the property holds for all sorted lists. 

-}
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)


prop_listOrdered :: String -> Bool
prop_listOrdered xs = listOrdered xs == isOrdered xs

isOrdered :: (Ord a) => [a] -> Bool
isOrdered xs = and (zipWith (<=) xs (tail xs))

{-
### Problem 5 - QuickCheck 
Given a datatype `Module`, use `Gen` from the `Test.QuickCheck` module to generate random values of `Module` with equal probabilities. 

-}


moGen :: Gen Module
moGen = do
 oneof [return $ THE1, return $ SOF1, return $ THE2, return $ SOF2, return $ SYS1, return $ DAT1, return $ HCI1]

instance Arbitrary Module where
 arbitrary = moGen

-- sample moGen

{-
### Problem 6 - QuickCheck 
Given that a student has type: `newtype Student = Student [(Module, Maybe Int)]`, use `Gen` to generate random values of type `Student`.
Note: a randomly generated `Student` can have the same `Module` multiple time and mark outsite the range 0-100.

-}


instance Arbitrary Student where
 arbitrary = do
   list <- arbitrary
   return $ Student list
-- sample $ (arbitrary :: Gen Student)


