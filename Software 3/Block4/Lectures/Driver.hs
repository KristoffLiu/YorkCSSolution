module Driver where
import N.AbstractSyntax -- abstract syntax of N as a data definition & pretty-printer
import N.Interpreter -- run M
import M.AbstractSyntax -- abstract syntax of Mas a data definition & pretty-printer
import M.Interpreter -- run M
import N2M.Compiler -- compile N into M
import Examples -- of N code

------------------------
-- Example N code in a separate file

explore :: (M -> String) -> Int -> (N -> IO ())
explore f n = putStrLn . either id f . compile n

rcn :: Int -> N -> IO () -- number-of-registers N-code
rcn = explore M.Interpreter.interpret

scn :: Int -> N -> IO () -- number-of-registers N-code
scn = explore M.AbstractSyntax.prettyprint
