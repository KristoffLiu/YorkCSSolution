```haskell
module Formative2Soln where
```
Jeremy Jacob 21 Dec 2020
A possible solution to SOF3/Formative 2:
The Royal Game of Ur
```haskell

```
Types and useful values
```haskell
data Player = Red | Green deriving (Eq, Show)
opponent :: Player -> Player
opponent    Red     = Green
opponent    Green   = Red

data Position = Start | OnBoard Square | Home deriving (Eq, Show)
instance Enum Position where -- cannot be derived
  fromEnum Start        = 0
  fromEnum (OnBoard sq) = 1 + fromEnum sq
  fromEnum Home         = 15
  toEnum 0 = Start
  toEnum 15 = Home
  toEnum n = OnBoard (toEnum (n - 1))
data Square = Sq_1 | Sq_2 | Sq_3 | Sq_4
            | Sq_5 | Sq_6 | Sq_7 | Sq_8 | Sq_9 | Sq10 | Sq11 | Sq12
            | Sq13 | Sq14
            deriving (Eq, Enum, Show)
newtype Placing = Placing (Position -> Player -> Int)
data GameState = GameState Placing Player

notHome, shared, rosette :: [Position]
notHome          = [Start .. OnBoard Sq14]
shared           = [OnBoard Sq_5 .. OnBoard Sq12]
rosette          = [OnBoard Sq_4, OnBoard Sq_8, OnBoard Sq14]
sharedRosette :: Position
sharedRosette  = OnBoard Sq_8
piecesPerPlayer, diceMin, diceMax :: Int
piecesPerPlayer = 7
```
A different kind of die.  Essentially four coins tossed that could be
1 (heads) or 0 (tails) each, and the value of the "throw" is the sum.

This gives probabilities:
0  1/16
1  4/16 = 1/4
2  6/16 = 3/8
3  4/16 = 1/4
4  1/16
```haskell
diceMin         = 0
diceMax         = 4

```
When calculating the new position of a token, a move "beyond" home, is
a move to home.
```haskell
newpos :: (Int, Position)              -> Position
newpos    (n, pos) = toEnum (((fromEnum pos) + n) `min` 15)

```
The initial state has both players with all their tokens at the start.
The first move belongs to the "red" player.
```haskell
initGS :: GameState
initGS  = GameState (Placing initPos) Red
  where
    initPos Start _ = piecesPerPlayer
    initPos _     _ = 0

```
A move of `n` steps from position `s` by player `p` is valid if:
1. The number of steps is in the range 0 up to 4 inclusive.
2. It is not a move from Home.
3. There is a piece belonging to the player on the chosen position.
4. There is not already a piece belonging to p in the new position,
   unless the new position is "home".
5. The new position is not the shared rosette occupied by the other player.

```haskell

possibleMoves :: GameState -> Int -> [Position]
possibleMoves (GameState (Placing b) p) n = filter (valid n) notHome
  where
    valid n s =
      diceMin <= n && n <= diceMax
      && b s p /= 0
      && (np == Home          || emptytarget p)
      && (np /= sharedRosette || emptytarget (opponent p))
      where
        np = newpos (n, s)
        emptytarget = (== 0) . b np

```
1. If there are valid moves with the current die roll:
  1. The current player chooses one.
  2. The player's token is moved from the chosen position to the new position.
  3. If the new position is a shared square, and it is occupied by the
     other player then the other player's piece returns to the start.
  4. The next player is the other player, unless the new position is a rosette.
2. If the die roll has no valid moves, the next player is the other player

```haskell
move :: GameState              -> (Int, Position) -> GameState
move  g@(GameState c@(Placing b) p) m@(n,   s)
  | null posMov     = GameState c                  (opponent p)
  | s `elem` posMov = GameState (Placing newboard) (newplayer p)
  | otherwise       = g 
  where
    posMov = possibleMoves g n
    -- "tp": this player; "op": other player
    newboard s' p' | tp s      = bsp - 1
                   | tp np     = bsp + 1
                   | op np     = 0
                   | op Start  = bsp + 1
                   | otherwise = bsp
      where
        bsp = b s' p' -- number of player p' pieces in position s' on placing b
        tp q = s' == q && p' == p
        op q = s' == q && p' == opponent p && np `elem` shared && b np p' == 1
    newplayer | np `elem` rosette = id
              | otherwise         = opponent
    np = newpos m

```
The game is over when one player gets all its tokens to "home".
```haskell
gameOver :: GameState               -> Maybe Player
gameOver    (GameState (Placing b) _)
  | allHome Red   = Just Red
  | allHome Green = Just Green
  | otherwise     = Nothing
  where
    allHome = (== piecesPerPlayer) . b Home

```
Interpretive interface
```haskell

winner :: [(Int, Position)] -> Maybe Player
winner = gameOver . last . scanl move initGS -- input list must be finite

------------------------------------------------------------------------

```
Interactive interface
```haskell

placing :: Placing -> Position -> Player -> Int
placing (Placing b) = b

ppB :: Placing -> String -- prettyprint Placing
ppB b =
  unlines
  ["Red start:   " ++ show (placing b Start Red) ++ replicate 5 ' ' ++ "Red home:   " ++ show (placing b Home Red),
   houter,
   privaterow [Red],
   hinner,
   cells [Red, Green] [Sq_5 .. Sq12],
   hinner,
   privaterow  [Green],
   houter,
   "Green start: " ++ show (placing b Start Green) ++ replicate 5 ' ' ++ "Green home: " ++ show (placing b Home Green)
  ]
  where
    privaterow ps =
      cells ps (reverse [Sq_1 .. Sq_4])
      ++ replicate 18 ' '
      ++ cells ps [Sq14, Sq13]
    kind pos = (if OnBoard pos `elem` rosette then ('*':) else (' ':)) (show pos)
    cells ps sqs = "| " ++ foldr (\s t->s ++ " | " ++ t) "" [kind pos ++ " " ++ take 1 (concat (map (\p-> if placing b (OnBoard pos) p == 1 then [head(show p)] else "") ps)++" ") | pos <- sqs]
    hinner = replicate 81 '-'
    houter = replicate 41 '-' ++ replicate 19 ' ' ++ replicate 21 '-'

ppG :: GameState -> String -- prettyprint GameState
ppG (GameState b p) = "Board:\n" ++ ppB b ++ "\nTo play: " ++ show p  



body :: GameState -> IO ()
body gs =
  do
    case gameOver gs of
      Nothing ->
        do
          putStrLn (ppG gs)
          putStrLn "Enter a dice roll (0-4)"
          roll' <- getLine
          let roll = read roll'
          let pm = possibleMoves gs roll
          if roll == 0 || null pm
            then do
              putStrLn "Missed turn"
              let GameState b p = gs
              body (GameState b (opponent p))
            else do
              putStrLn ("Tokens to move are: "++show (zip [0..] pm))
              putStrLn ("Enter id number of a token position (0-" ++ show (length pm - 1) ++")")
              sq' <- getLine
              body (move gs (roll, pm!!(read sq')))
      Just pl ->
        putStrLn ("Congratulations: " ++ show pl ++ " wins!")

play :: IO ()
play = body initGS

----------------------------- Kofi's tests
allRedHomeGS :: GameState
allRedHomeGS  = GameState (Placing curPos) Red
  where
    curPos Start Green = piecesPerPlayer
    curPos Home Red = piecesPerPlayer
    curPos _     _ = 0

allRedHomeGS' :: GameState
allRedHomeGS'  = GameState (Placing curPos) Green
  where
    curPos Start Green = piecesPerPlayer
    curPos Home Red = piecesPerPlayer
    curPos _     _ = 0

allGreenHomeGS :: GameState
allGreenHomeGS  = GameState (Placing curPos) Green
  where
    curPos Start Red = piecesPerPlayer
    curPos Home Green = piecesPerPlayer
    curPos _     _ = 0

allGreenHomeGS' :: GameState
allGreenHomeGS'  = GameState (Placing curPos) Red
  where
    curPos Start Red = piecesPerPlayer
    curPos Home Green = piecesPerPlayer
    curPos _     _ = 0

allOnBoardGS :: GameState
allOnBoardGS  = GameState (Placing curPos) Red
  where
    curPos (OnBoard Sq_1) Red = 1
    curPos (OnBoard Sq_2) Red = 1
    curPos (OnBoard Sq_3) Red = 1
    curPos (OnBoard Sq_4) Red = 1  
    curPos (OnBoard Sq12) Red = 1
    curPos (OnBoard Sq13) Red = 1
    curPos (OnBoard Sq14) Red = 1    
    curPos (OnBoard Sq_1) Green = 1
    curPos (OnBoard Sq_2) Green = 1
    curPos (OnBoard Sq_3) Green = 1
    curPos (OnBoard Sq_4) Green = 1  
    curPos (OnBoard Sq11) Green = 1
    curPos (OnBoard Sq13) Green = 1
    curPos (OnBoard Sq14) Green = 1 
    curPos _     _ = 0    

allOnBoardGS' :: GameState
allOnBoardGS'  = GameState (Placing curPos) Green
  where
    curPos (OnBoard Sq_1) Red = 1
    curPos (OnBoard Sq_2) Red = 1
    curPos (OnBoard Sq_3) Red = 1
    curPos (OnBoard Sq_4) Red = 1  
    curPos (OnBoard Sq12) Red = 1
    curPos (OnBoard Sq13) Red = 1
    curPos (OnBoard Sq14) Red = 1    
    curPos (OnBoard Sq_1) Green = 1
    curPos (OnBoard Sq_2) Green = 1
    curPos (OnBoard Sq_3) Green = 1
    curPos (OnBoard Sq_4) Green = 1  
    curPos (OnBoard Sq11) Green = 1
    curPos (OnBoard Sq13) Green = 1
    curPos (OnBoard Sq14) Green = 1 
    curPos _     _ = 0   
