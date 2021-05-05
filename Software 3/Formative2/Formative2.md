```haskell
module Formative2 where

```
# SOF3 Formative 2

This formative exercise asks you to implement components for playing:
                       The Royal Game of Ur

The Royal Game of Ur is the oldest known board game.  The oldest known
board and pieces are around 4,500 years old.  The only rule book we
have, which is for versions of the game rather than the original game,
is a youthful 2,500 years or so old.  This rule book is written in
cuneiform letters on a clay tablet, and currently resides in the
British Museum.  It was translated by one of the British Museum's
curators, Irving Finkel.  There are YouTube videos of him
[discussing](https://youtu.be/wHjznvH54Cw) and [playing the game with
Tom Scott](https://youtu.be/WZskjLq040I) (Tom Scott is a linguistics
graduate from the University of York).

The rules are not completely known, but this exercise uses rules based
on the [Finkel-Scott match](https://youtu.be/WZskjLq040I).

The game is a race game, such as ludo or backgammon for two players,
that we will call `Red` and `Green`.
```haskell
data Player = Red | Green deriving (Eq, Show)

```
## Q1 [2 marks]
Implement a function that returns a player's opponent.
```haskell
opponent :: Player -> Player
opponent = undefined

```
A board contains 14 _logical_ squares, arranged as 20 _physical_ squares.

```
-----------------------------------------                   ---------------------
| *Sq_4   |  Sq_3   |  Sq_2   |  Sq_1   |                   | *Sq14   |  Sq13   | 
---------------------------------------------------------------------------------
|  Sq_5   |  Sq_6   |  Sq_7   | *Sq_8   |  Sq_9   |  Sq10   |  Sq11   |  Sq12   | 
---------------------------------------------------------------------------------
| *Sq_4   |  Sq_3   |  Sq_2   |  Sq_1   |                   | *Sq14   |  Sq13   | 
-----------------------------------------                   ---------------------
```

```haskell
data Square = Sq_1 | Sq_2 | Sq_3 | Sq_4
            | Sq_5 | Sq_6 | Sq_7 | Sq_8 | Sq_9 | Sq10 | Sq11 | Sq12
            | Sq13 | Sq14
            deriving (Eq, Enum, Show)
```
The red player's pieces move along the top and middle rows, in
numerical order, while the green player's pieces move similarly along
the bottom and middle rows.  Squares 1-4 and 13-14 are private, but
squares 5-12 are _shared_ and where, in Irving Finkel's words, the two
players are "at war".

There are special squares: the private squares 4 and 14, and the
shared square 8.  On a real board these are decorated with a rosette,
indicated here by an asterisk ('*').

Each player has seven (7) identical pieces.  Each piece has a
position:
* waiting to enter the board (at the `Start`),
* on a square on the board, or
* having reached `Home`.

```haskell
data Position = Start | OnBoard Square | Home deriving (Eq, Show)
```
A game state consists of:
* a placing of pieces in positions, and
* the identity of the next player.

```haskell
type Placing = () -- UNDEFINED TYPE
placing :: Placing -> Position -> Player -> Int
placing = undefined
data GameState = GameState Placing Player

```
## Q2 [10 marks]
Implement the type `Placing` and the function `placing`.
* When implementing the type you may change its declaration keyword to
  `newtype` or `data` if appropriate.
* You may derive type classes as part of the type definition, if
  `newtype` or `data`.
* The expression `placing b p q` should return the number of pieces
  player `q` has at position `p` in placing `b`.

Initially all of a player's pieces are at the `Start` position,
waiting to enter the board.  The first move is by the red player.

## Q3 [2 marks]
Implement the initial game state, `initGS`.
```haskell
initGS :: GameState
initGS = undefined

```
A dice roll is, essentially, the number of heads in four fair coin tosses.
This gives the following probabilities.

| Roll | Probability |
| ---- | ----------- |
| 0    | 1/16        |
| 1    | 4/16 = 1/4  |
| 2    | 6/16 = 3/8  |
| 3    | 4/16 = 1/4  |
| 4    | 1/16        |

You are **not** asked to implement a dice; instead the value will be
given by an external oracle.

A player may move any one of their pieces by the value of the dice
roll, subject to many conditions.

A move of `n` steps from position `s` by player `p` is valid if:
1. The number of steps is in the range 0 up to 4 inclusive.
2. Position `s` is not `Home`.
3. There is a piece belonging to the player in position `s`.
4. There is not already a piece belonging to `p` in the new position,
   unless the new position is `Home`.
5. The new position is not the shared rosette occupied by the opponent.
6. A move "beyond" `Home` is valid (for example, a piece on Square 13
   may be moved with rolls of 3 and 4, as well as 1 and 2).

## Q4 [6 marks]
Implement the function which, given a game state and a dice roll,
returns a list of squares from which a move is valid.
```haskell
possibleMoves :: GameState -> Int -> [Position]
possibleMoves = undefined
```
1. If there are valid moves with the current dice roll:
  1. The current player chooses one.
  2. The player's token is moved from the chosen position to the new position.
  3. If the new position is a shared square, and it is occupied by the
     other player then the other player's piece returns to the start.
  4. If the new position is "beyond" `Home`, it is taken to be
     `Home`.  (For example, a piece on Square 13 may be moved to home
     with a roll of 2, 3 or 4.)
  5. The next player is the opponent, unless the new position is a
     rosette, in which case the current player has another roll.
2. If the dice roll has no valid moves, the next player is the opponent.

## Q5 [8 marks]
Implement the function which, given a game state and a dice
roll/position pair, returns the new game state.
```haskell
move :: GameState -> (Int, Position) -> GameState
move = undefined
```
The game is over when one player gets all its tokens to "home".

We will model this by a function that, given a game state, returns
* `Nothing` when neither player has won, and
* `Just p` when player `p` has won (note: draws are not possible).

## Q6 [4 marks]
```haskell
gameOver :: GameState               -> Maybe Player
gameOver = undefined
```
## Q7 [8 marks]

Given a list of dice roll/position pairs representing moves, implement
a function, `winner`, that returns the winner, if there is one, of
that sequence of moves.
```haskell
winner :: [(Int, Position)] -> Maybe Player
winner = undefined

```
If you wish, you can load the file `PlayRGU.hs` into `ghci`.  It
imports your file, `Formative2.hs` and provides a function `play` that
allows you to have an interactive match.  You must provide your own dice.

**WARNING** the interface does no error checking, and will crash or
  otherwise behave badly, if you enter an unexpected value.
```haskell

```