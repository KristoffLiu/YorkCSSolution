module Q1iv where -- 6 marks
import Q1i 
import Q1ii
import Q1iii 

{-
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

Implement the function which, given a game state and a dice roll,
returns a list of squares from which a move is valid.

Your solution should satisfy:
-}

possibleMovesTest :: Bool
possibleMovesTest = 
  (possibleMoves initGS 0        == []) &&
  (possibleMoves initGS 4        == [Start]) &&
  (possibleMoves allOnBoardGS 1  == [OnBoard Sq_4,OnBoard Sq14]) &&
  (possibleMoves allOnBoardGS' 1 == [OnBoard Sq_4,OnBoard Sq11,OnBoard Sq14]) &&
  (possibleMoves allOnBoardGS 2  == [OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq13,OnBoard Sq14]) &&
  (possibleMoves allOnBoardGS' 2 == [OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq13,OnBoard Sq14]) &&
  (possibleMoves allOnBoardGS 0  == [OnBoard Sq_1,OnBoard Sq_2,OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq12,OnBoard Sq13,OnBoard Sq14]) &&
  (possibleMoves allOnBoardGS' 0 == [OnBoard Sq_1,OnBoard Sq_2,OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq11,OnBoard Sq13,OnBoard Sq14]) &&
  (possibleMoves allOnBoardGS 4  == [OnBoard Sq_1,OnBoard Sq_2,OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq12,OnBoard Sq13,OnBoard Sq14]) &&
  (possibleMoves allOnBoardGS' 4 == [OnBoard Sq_1,OnBoard Sq_2,OnBoard Sq_3,OnBoard Sq_4,OnBoard Sq11,OnBoard Sq13,OnBoard Sq14]) 

minDice, maxDice :: Int
minDice = 0
maxDice = 4



sharedRosette :: Position
sharedRosette = OnBoard Sq_8

notHome, shared, rosettes :: [Position]
notHome = [Start .. OnBoard Sq14]
shared = [OnBoard Sq_5 .. OnBoard Sq12]
rosettes = [OnBoard Sq_4, OnBoard Sq_8, OnBoard Sq14]

possibleMoves :: GameState -> Int -> [Position]
possibleMoves (GameState (Placing plc) player) n = filter isValid notHome
  where
    -- element元素
    isValid e = 
      minDice <= n && maxDice >= n
      && plc e player /= 0
      && (isTargetEmpty player || np == Home)
      && (np /= sharedRosette || isTargetEmpty (opponent player))
      where 
        np = newPosition e n
        isTargetEmpty p = (plc np p) == 0

newPosition :: Position -> Int -> Position
newPosition e n = toEnum (((fromEnum e) + n) `min` 15)

instance Enum Position where
  fromEnum Start = 0
  fromEnum (Onboard sq) = 1 + fromEnum sq
  fromEnum Home = 15
  toEnum 0 = Start
  toEnum 15 = Home
  toEnum n = Onboard (toEnum (n-1))




-- map    fx [list]
-- filter fx [list] (fx 布尔值)
-- Placing (Position -> Player -> Int)






{-

For the purposes of `possibleMovesTest`, you will need to define `allOnBoardGS :: GameState` and `allOnBoardGS' :: GameState`. Assume the following is the state of the board for `allOnBoardGS :: GameState` and `allOnBoardGS' :: GameState`. Where `Red` is the next player in the case of `allOnBoardGS` and `Green` the next player in the case of `allOnBoardGS'`. `SqXX R` means the square is occupied by player `Red`.
```
---------------------------------------------------------------------------------------
| *Sq_4 R  |  Sq_3 R  |  Sq_2 R  |  Sq_1 R  |                   | *Sq14 R  |  Sq13 R  | 
---------------------------------------------------------------------------------
|  Sq_5    |  Sq_6    |  Sq_7    | *Sq_8    |  Sq_9   |  Sq10   |  Sq11 G  |  Sq12 R  | 
---------------------------------------------------------------------------------
| *Sq_4 G  |  Sq_3 G  |  Sq_2 G  |  Sq_1 G  |                   | *Sq14 G  |  Sq13 G  | 
---------------------------------------------------------------------------------------
```
-}
