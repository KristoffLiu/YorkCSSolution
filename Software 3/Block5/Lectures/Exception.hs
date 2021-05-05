module Exception where
import Control.Exception
import System.IO
import System.IO.Error
import Control.Monad (forever)
{-
# Exceptions
# Jeremy Jacob 6 April 2021

## Introduction

We have seen that Haskell encourages a value-oriented approach to
exceptional circumstances, through the type constructors `Maybe` and
`Either`, rather than the statement-oriented approach of most
languages.

The Haskell approach gives a structured method, about which we can
reason, and the syntactic sugar of monads can take some of effort away.

Recall the example:
-}
safediv :: Int -> Int -> Maybe Int
safediv    _      0    = Nothing
safediv    m      n    = Just (m `div` n)

exampleA :: Int -> Maybe Int
exampleA    n    = do -- (4 `safediv` ((-n)+3)) + 1; fails on input 3
  x <- pure (negate n)
  x <- pure (x + 3)
  x <- 4 `safediv` x -- fails if x==0
  x <- pure (x + 1)
  return x
{-
This technique works well for computation under the programmer's
control.  However, as soon as the code needs to communicate with its
environment the programmer may no longer assume anything.  To quote
C. A. R. "Tony" Hoare,
> "Always assume that the environment is junk",

or as Oscar Wilde said
> "To expect the unexpected shows a thoroughly modern intellect".

To have a thoroughly modern intellect and expect the unexpected _at
the boundaries of our control_ we need a tool to help us:
**exceptions**.

## Haskell's exception mechanism

The exception mechanism should **only** be used for interactions at
the boundary of the programmer's control.  Code with exceptions in is
harder to reason about than exception-free code.

The mechanism requires import of the module
[`Control.Exception`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html),
which provides the type class `Exception`, types which are instances
of `Exception`, such as `IOException`, and functions such as `catch`,
`handle`, `try` and `bracket`.  None are available through `Prelude`,
forcing the programmer to be explicit that they are using a more
dangerous construct.

The basic mechanism for catching and dealing with an exception is
```haskell
catch :: Exception e	 
         => IO a  -- main computation
         -> (e -> IO a) -- handler for exceptions arising in main computation
         -> IO a -- resulting computation
```
Note that this only deals with exceptions in the `IO` monad, which is
where the boundary to the program lies: you should deal with
everything else using `Maybe` or `Either`.  The expression:
```haskell
expr `catch` exceptionHandler
```
attempts to evaluate `expr`; if no exception is raised then its value
is returned in the normal way; however if evaluation of `expr` raises
the exception `boom`, then the result of evaluating `exceptionHandler
boom` is returned instead.

The most useful exception type is `IOException`, which has the synonym
`IOError`.

## An example: a missing file to be read

### Without handling

Recall that the problem Q4 for Block 4 had a question that asked you
to solve a problem assuming the existence of a file.  We can use
exceptions to deal with the case of a missing file.  Here is a
massively restructured and simplified version of the answer:
-}
fileMustExist ::    FilePath -- filename
                 -> String   -- new content
                 -> IO ()
fileMustExist fp new = do
  current <- readFile fp
  putStrLn ("Current contents of \"" ++ fp ++ "\": \"" ++ current ++ "\"")
  writeFile fp new
  putStrLn ("Writing \"" ++ fp ++ "\" with: \"" ++ new ++ "\"")  
{-
The expression `fileMustExist "example.txt" "NEW"` will raise an
exception if the file `"example.txt"` does not exist, or if it does
exist, but appropriate permissions do not exist.

### Catching exceptions at the outermost level
It is better if the exception is caught and handled.  Depending on
what effect is wanted, it can be handled in various ways.  First, we
could treat the whole evaluation as broken or not, and use the
exception to trigger an error message.
-}
abortOnException ::    FilePath -- filename
                    -> String   -- new content
                    -> IO ()
abortOnException fp new = do
  fileMustExist fp new `catch` readKO
  where
    -- NB: type statement for readKO is necessary
    readKO :: IOException -> IO ()
    readKO e = do
      putStrLn ("Attempt to access \"" ++ fp
                ++ "\" has raised exception: " ++ show e)
      putStrLn "Evaluation aborted"

{-
### Finer-grained catching of exceptions

If there is a sensible default content for a non-existent file or
unreadable file, then we could use a function such as
`sensibleDefault`, which also illustrates handling any exceptions that
may arise by attempting to write a file:
-}
sensibleDefault ::    FilePath -- file name
                   -> String   -- default value
                   -> String   -- new value
                   -> IO ()
sensibleDefault fp df new = do
  content <- readFile fp `catch` defaultContent
  putStrLn ("Existing contents of \"" ++ fp ++ "\": " ++ content)
  exceptionOnWrite `handle` do
      putStrLn ("Attempting to update \"" ++ fp
                ++ "\" to contain \"" ++ new ++ "\"")
      writeFile fp new
  where
    -- NB: type statements for handlers are necessary
    defaultContent :: IOException -> IO String
    defaultContent _ = pure df
    exceptionOnWrite :: IOException -> IO ()
    exceptionOnWrite e = do
      putStrLn ("Attempt to write to \"" ++ fp
                ++ "\" has raised exception: " ++ show e)
      putStrLn "Evaluation aborted"
{-
Just for the sake of illustration, this uses the function `handle`
instead of `catch` to deal with the second file access.  The only
difference is that `handle` takes its parameters in the opposite order
to `catch`:
```haskell
handle = flip catch
```

### Exception catching structures
The function `handle` is a convenience that allows us to easily build
special purpose exception-handling functions by partial application,
such as:
-}
unwritableFile :: FilePath -> IO () -> IO ()
unwritableFile    fp        = handle exceptionOnWrite
  where
    -- The type declaration for handlers is necessary
    exceptionOnWrite :: IOException -> IO ()
    exceptionOnWrite e | isPermissionError e = do
                           putStr "No write permission for \""
                           putStr fp
                           putStrLn "\": contact IT support"
                           putStrLn "All your hard work has been thrown away!"
                       | otherwise           = do
                           putStrLn (show e)
                           putStrLn "Evaluation aborted"
{-
Note that we are using facilities of `System.IO.Error` to analyse the
value of the `IOException`, in particular `isPermissionError`.

The function `sensibleDefault` does not give a good message if the
file does not exist.  This can be solved by a further extension of
`catch`, which embeds its result in the `Either` monad.  An
unexceptional value is tagged with `Right`, and an exception with
`Left`.  The rest of the program can evaluate the tag to decide how to
proceed; `either` is often a convenient way of doing this.

Also illustrated is the use of `unwritableFile`.
-}
prettyDefault ::    FilePath -- file name
                 -> String   -- default
                 -> String   -- new value
                 -> IO ()
prettyDefault fp df new = do
  content <- try (readFile fp)
  putStrLn (either
            ((const df) :: IOException -> String) -- declaration necessary to fix exception type
            (("Existing contents of \"" ++ fp ++ "\": ") ++)
            content)
  unwritableFile fp (do putStr "Updating \""
                        putStr fp
                        putStr "\" to contain \""
                        putStr new
                        putStrLn "\""
                        writeFile fp new)

{-
## Structured exception handling functions
The function `catch` and its variants `handle` and `try` are
relatively low-level.  For many tasks in the `IO` monad it is better
to use `bracket`, which gives a structured way of accessing resources
such as files.  It is of particular utility when using the lower-level
facilities provided by `System.IO` rather than the higher-level
`readFile` and `writeFile` in the examples above.

The expression
```haskell
bracket acquire release body
```
first evaluates `acquire`, then `body` and finally `release`.  If an
exception is raised during `acquire` or `body` then `release` is
evaluated to tidy up, and the exception re-raised for the context of
`bracket acquire release body` to catch.  In the case of resources
which are system files, there is a specialisation of `bracket` called
`withFile`, in module
[`System.IO`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html).
```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile name mode = bracket (openFile name mode) hClose
```

An example is copying some lines of one file to a second file.  It
throws an exception when the end of the source file is reached, or any
other exception is raised, but shuts down both files cleanly.
-}
filterFile ::    (String -> Bool)   -- to select lines
              -> FilePath           -- source file
              -> FilePath           -- destination file
              -> IO ()
filterFile select source destination =
  withFile source ReadMode
           (\ sourceHdl ->
              withFile destination WriteMode
                       (\ destHdl ->
                          forever (do -- forever interrupted by EOF exception
                                      line <- hGetLine sourceHdl
                                      if select line
                                        then hPutStrLn destHdl line
                                        else pure () -- do nothing
                                  )))
{-
We can use the utility to create a further utility to get the skeleton
consisting of only title lines of a Markdown file by:
-}
mdSkeleton :: FilePath -> FilePath -> IO ()
mdSkeleton  = filterFile isTitle
  where
    isTitle ('#':_) = True
    isTitle _       = False
{-
and then calling, for example
```haskell
mdSkeleton "source.md" "skeleton_for_source.md"
```
A nicer solution tests the source file for End-of-file (using
[`hIsEOF::handle -> IO
Bool`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:hIsEOF))
and does not generate an exception.  We leave that as an exercise.

## Raising (throwing) exceptions
There are, of course, mechanisms for throwing exceptions in
`Control.Exception`.  Because this is deprecated in normal code it is
not further discussed here.
-}
