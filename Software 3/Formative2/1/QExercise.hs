module QExercise where --
{-
Jeremy Jacob 21 Dec 2020
A possible solution to SOF3/Formative 2:
The Royal Game of Ur
-}

{-
Types and useful values
-}
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


