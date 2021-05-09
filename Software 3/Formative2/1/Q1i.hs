module Q1i where -- 2 marks

{-


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
-}
data Player = Red | Green deriving (Eq, Show)

{-

Implement a function that returns a player's opponent.

-}

opponent :: Player -> Player
opponent Red = Green
opponent Green = Red

