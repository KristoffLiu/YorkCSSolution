module Q1iii where -- 2 marks
import Q1i 
import Q1ii

{-
Initially all of a player's pieces are at the `Start` position,
waiting to enter the board.  The first move is by the red player.

1. Position = Start 

A game state consists of:
* a placing of pieces in positions, and
* the identity of the next player.
data GameState = GameState Placing Player
Placing = a placing of pieces in positions
    Placing = Placing (Position -> Player -> Int)
Player = Red


Implement the initial game state, `initGS`.
-}
piecesPerPlayer :: Int
piecesPerPlayer = 7

initGS :: GameState
initGS = GameState (Placing initPos) Red
    where 
        initPos Start _ = piecesPerPlayer
        initPos _     _ = 0

-- data GameState = GameState Placing Player
-- newtype Placing = Placing (Position -> Player -> Int)
    
