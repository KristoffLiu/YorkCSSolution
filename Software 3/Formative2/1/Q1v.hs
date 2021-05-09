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

-- Game State 游戏状态 每一回合的所有信息
move :: GameState -> (Int, Position) -> GameState
move g@(GameState c@(Placing plc) p) m@(n, pos)
   | null posMove = GameState c (opponent p)
   | p `elem` posMove = GameState () nextPlayer
   | otherwise = g
   where 
      posMove = possibleMoves g n -- <- Possible Positions
      nextPlacing pos' player' | thisPlayer pos = currentPlacing - 1
                               | thisPlayer np = currentPlacing + 1
                               | oppoPlayer np = 0
                               | oppoPlayer Start = currentPlacing + 1
                               | otherwise = currentPlacing
         where 
            thisPlayer q = q == pos' && player' == p
            oppoPlayer q = q == pos' && player' == (opponent p)
            currentPlacing = plc pos' player'
      nextPlayer  | np `elem` rosette = p
                  | otherwise = (opponent p)
      np = newPosition p n



-- a@b -> a as b