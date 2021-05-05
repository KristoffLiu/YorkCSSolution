module Q1vii where -- 8 marks
import Q1i 
import Q1ii

{-
You may use your answers to Q1iii, Q1iv, Q1v and Q1vi for `move`.
If you chose to do so, uncomment the following line
-}
-- import Q1iii 
-- import Q1iv 
-- import Q1v
-- import Q1vi
{-


Given a list of dice roll/position pairs representing moves, implement
a function, `winner`, that returns the winner, if there is one, of
that sequence of moves.

Your solution should satisfy:
-}

winnerTest :: Bool
winnerTest = 
  (winner [(4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (1, Start), (4, OnBoard Sq12), (0, OnBoard Sq_1)] == Nothing) &&
  (winner ( take 42 (cycle  [(4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (1, Start), (4, OnBoard Sq12), (0, OnBoard Sq_1)])) == Nothing) &&
  (winner ( take 42 (cycle  [(4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (0, Start), (4, OnBoard Sq12), (0, Start)])) == Just Red) &&
  (winner  [(0, Start), (4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (0, Start), (4, OnBoard Sq12)] == Nothing) &&
  (winner ( take 42 (cycle  [(0, Start), (4, Start), (4, OnBoard Sq_4), (4, OnBoard Sq_8), (0, Start), (4, OnBoard Sq12)])) == Just Green)


winner :: [(Int, Position)] -> Maybe Player
winner = undefined

{-
If you wish, you can load the file `PlayRGU.hs` into `ghci`.  It
imports your file, `Formative2.hs` and provides a function `play` that
allows you to have an interactive match.  You must provide your own dice.

**WARNING** the interface does no error checking, and will crash or
  otherwise behave badly, if you enter an unexpected value.
-}
