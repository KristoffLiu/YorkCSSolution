module Q1vi where -- 4 marks
import Q1i 
import Q1ii

{-
You may use your answers to Q1iii and Q1iv for `move`.
If you chose to do so, uncomment the following line
-}
-- import Q1iii 
-- import Q1iv -- only if you defined `allOnBoardGS :: GameState` for testing

{-
The game is over when one player gets all its tokens to "home".

We will model this by a function that, given a game state, returns
* `Nothing` when neither player has won, and
* `Just p` when player `p` has won (note: draws are not possible).

Your solution should satisfy:

-}

gameOverTest :: Bool
gameOverTest = 
  (gameOver initGS == Nothing) &&
  (gameOver allRedHomeGS == Just Red) &&
  (gameOver allGreenHomeGS == Just Green) &&
  (gameOver allOnBoardGS == Nothing)


gameOver :: GameState               -> Maybe Player
gameOver = undefined

{-
For the purposes of `gameOverTest`, you will have to define `allGreenHomeGS :: GameState` to reflect all player `Red` pieces (tokens) at `Home` and `allRedHomeGS :: GameState` when all player `Green` pieces are at `Home`.

-}
