module PlayRGU (play) where
import Formative2

import Q1i 
import Q1ii
import Q1iii 
import Q1iv 
import Q1v
import Q1vi
import Q1vii

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
    kind pos = (if pos `elem` [Sq_4, Sq_8, Sq14] then ('*':) else (' ':)) (show pos)
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
