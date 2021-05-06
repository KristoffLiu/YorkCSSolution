module SnakesLadders where

{-
# Software 3
# A programming example: Snakes and Ladders library module
# Jeremy Jacob 13 Oct 2020

## Problem description

Snakes and ladders is a board game that was originally developed in
India, to illustrate the difficulties (snakes) and short-cuts
(ladders) on the approach to Nirvana.

Players move around a board (Typically of 100 positions) by moving
from their current position by a random number generated from a die
roll.  In many cases the next position is found by adding the die roll
to the current position; but in some cases the natural next position
is the foot of a "ladder" or the head of "snake".  In these cases the
actual next position is the top of the ladder or the tail of the
snake; ladders always represent a forward jump and snakes a backward
jump.

We need to implement a function that describes a `move` on a `Board`
in a particular configuration or state of the game (`GameState`),
following a roll of a die (represented as a value of type `Int`),
resulting in a new `GameState`.
-}

type Move = Board -> GameState -> Int -> GameState

{-
What follows is a simple model, using only types and values defined in
`Prelude`.

## The board

We start with a model of the `Board`.  It has a number of squares,
that follow each other in a virtual line, whatever the layout of a
physical board.  We can model this by a subset of the Integers,
starting at 0 and ending at the last square.
-}

lastSquare :: Int
lastSquare = 9 -- 99 in a real board

squares :: [Int]
squares = [0..lastSquare]

{-
Each square is either:
* a jump (forward if a ladder, backward if a snake) to another square,
  or
* not a jump in either direction.

We can model this by a function from square number to the destination
square.  If this is a:
* jump, then the result will be different to the square
* not a jump, then the result is the same as the square

We will give a synonym to this function type, and ensure that it
cannot be confused with any other type:
-}

newtype Board = Board (Int -> Int)

{-
It is useful to be able to convert a board into a printable list.
-}

board2list :: Board    -> [(Int, Int)]
board2list    (Board b) = [(sq, b sq) | sq <- squares]

{-
Most values of type `Board` do not represent valid boards.  To be valid:
* The first and last squares must not be jumps
* The other squares may be jumps, but they must be jumps to squares on
  the board.

-}

validBoard :: Board -> Bool
validBoard (Board b)
  =       b lastSquare == lastSquare
       && b 0          == 0
       && and [0<=s && s<=lastSquare | s <- map b (init (tail squares))]

{-
We can determine the lists of squares which are snakes, and similarly
ladders:
-}

ladders, snakes :: Board -> [Int]
ladders (Board b) = [sq | sq <- squares, b sq > sq]
snakes (Board b) = [sq | sq <- squares, b sq < sq]

{-
We can define a board with no jumps:
-}

jumpless :: Board -- board with no jumps
jumpless = Board id

{-
and a function to add jumps to a board:
-}

jump :: Int -> Int -> Board -> Board -- add jump to board
jump from to (Board b) = Board c
  where
    c sq | sq==from  = to
         | otherwise = b sq
{-
Here is an example of a board with one snake and one ladder.

Load this file, and check that it is a valid board, and has one snake
and one ladder.
-}

example :: Board
example = jump 3 8 (jump 7 4 jumpless)

{-
## Players

There are many ways of modelling players.  All we need are a set of
labels, so we will use a simple data type of four labels.  We could
easily allow for more players by adding more labels; if we want
massively more, then we could use `Char`, or even `Int` to model names
of players.

We need to be able to test `Players` for equality, in a standard way.
It is useful to be able to enumerate players and print them.
-}

data Player = Red | Yellow | Green | Blue deriving (Eq, Enum, Show)

{-
It is useful to have a list of all players, in the order of their
turn, and a next turn function.
-}

players :: [Player]
players  = [Red .. Blue]
nextTurn :: Player -> Player
nextTurn    Blue    = Red
nextTurn    p       = succ p 

{-
## The state of the game

Next we model the state of a game.  Again we can do this by a function
that tells us on which square the player is occupying, and a value
telling us which player is next to play.

As with `Board` it is useful to be able to convert a `GameState` to a
printable value.
-}

data GameState = GameState (Player -> Int) Player

gs2list :: GameState       -> [(String, Int)]
gs2list (GameState pos cur) = [(showPlayer p, pos p) | p<-players]
  where
    showPlayer p = (if p==cur then ('*':) else id) (show p)

{-
The initial state, at the start of a game, is easily modelled as the
constant function that maps every player to Square 0, and declares the
next turn to belong to Red.
-}

initGS :: GameState
initGS = GameState (const 0) Red

{-
We also need to know when a `GameState` represents a final position.
There are many ways to do this.  The algorithm below checks if there
any players whose position is equal to `lastSquare`.
-}

gameOver :: GameState -> Bool
gameOver (GameState pos _) = any ((==lastSquare) . pos) players

{-
## The move function

There are two versions of move, depending on how rolls that move a
player beyond the last square are treated.  A roll that moves past the
last square may be taken as a roll:
1. exactly reaching the last square (this is implemented below), or
2. of zero, and the player does not move (this version is left as an
   exercise).

-}

move :: Move
move (Board b) (GameState pos cur) n = GameState new (nextTurn cur)
  where
    new q | cur==q    = b (lastSquare `min` (pos cur + n))
          | otherwise = pos q

{-
## Putting things together

The library is now complete.  There is enough here for a user of the
library to implement an interface.

However, as an illustration, here is a simplistic way of implementing
a whole game.  Ideally the material below would be in a separate
module, which means a separate file (one module per file).

First we generate the list of all states reached in the game, given
all the rolls of the die.

This uses the `Prelude` function, `scanl`.
Suppose `xs = [x0, x1, x2,...]`, then
`scanl f z xs = [z, f z x0, f (f z x0) x1, f (f (f z x0) x1) x2, ...]`
That is, the value in the result at position `k` can be computed as
`foldl f z` applied to `take k xs` (but `scanl` is more efficient).

As a concrete example, consider: `take 9 (scanl (+) 0 [1,..])`.

The initial value is the initial game state, `initGS`.  The list of
values to scan over is the list of die rolls.
-}

trace :: Board -> [Int] -> [GameState]
trace    b      = scanl (move b) initGS

{-
However, an infinite list of `GameState` does not tell us who won the
game.  We can examine a game trace to find the first state in which
there is a winner, and then extract the name of the winner.
-}

winner :: [GameState] -> Player
winner    gs
  = head [p | p<-players, winningPos p == lastSquare]
  where GameState winningPos _ = head (dropWhile (not . gameOver) gs)

{-
We can put these together, to get a function that has input a board,
and a sufficiently long list of die rolls, and output the winning
player.
-}

game :: Board -> [Int] -> Player
game    b      = winner . trace b

{-
As examples, here are games played on the `example` board, where:
-}

eg1 = game example (repeat 1) -- all rolls are 1
eg6 = game example (repeat 6) -- all rolls are 6
-- rolls follow the pattern [2, 4, 6, 2, 4, 6, 2, 4, 6...]
eg246 = game example (cycle [2, 4, 6])

{-
To get a random sequence of rolls we would need to use the library
module `System.Random`.
-}
