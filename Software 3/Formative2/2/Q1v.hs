module Q1v where -- 8 marks
import Q1i 
import Q1ii

{-
You may use your answers to Q1iii and Q1iv for `move`.
If you chose to do so, uncomment the following line
-}
-- import Q1iii 
-- import Q1iv

{-
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


Implement the function which, given a game state and a dice
roll/position pair, returns the new game state.
-}
move :: GameState -> (Int, Position) -> GameState
move = undefined
