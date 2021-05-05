module Q1ii where -- 10 marks
import Q1i 

{-
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

-}
data Square = Sq_1 | Sq_2 | Sq_3 | Sq_4
            | Sq_5 | Sq_6 | Sq_7 | Sq_8 | Sq_9 | Sq10 | Sq11 | Sq12
            | Sq13 | Sq14
            deriving (Eq, Enum, Show)
{-
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

-}
data Position = Start | OnBoard Square | Home deriving (Eq, Show)
{-
A game state consists of:
* a placing of pieces in positions, and
* the identity of the next player.

-}
type Placing = () -- UNDEFINED TYPE
placing :: Placing -> Position -> Player -> Int
placing = undefined
data GameState = GameState Placing Player

{-

Implement the type `Placing` and the function `placing`.
* When implementing the type you may change its declaration keyword to
  `newtype` or `data` if appropriate.
* You may derive type classes as part of the type definition, if
  `newtype` or `data`.
* The expression `placing b p q` should return the number of pieces
  player `q` has at position `p` in placing `b`.

-}
