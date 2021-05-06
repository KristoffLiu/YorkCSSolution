module Q1viiia where -- 2 marks

{-
Consider the type of Stage 1 students record `CS1`:
-}

data CS1 = Student {name :: String, the1, sof1, sys1 :: Int}

{-

A section of records for five Stage 1 students are available in the database `s1Db`. Each record for a student has the student's name and marks for three modules (THE1, SOF1 and SYS1).

-}
s1Db :: [CS1]
s1Db = [Student {name = "Beth", the1 = 65, sof1 = 58, sys1 = 79},
       Student {name = "Adam", the1 = 55, sof1 = 68, sys1 = 61},
       Student {name = "Lisa", the1 = 60, sof1 = 72, sys1 = 65},
       Student {name = "Will", the1 = 71, sof1 = 52, sys1 = 49},
       Student {name = "Mark", the1 = 67, sof1 = 78, sys1 = 50}]

{-

Write a function `the1Mk` that takes any `CSYear1` records like `y1Db` and returns a list of pairs of the name and THE1 mark of each student in the same order as they appear in the database.

Your solution should satisfy:
-}

test1MK :: Bool
test1MK = the1Mk s1Db == [("Beth",65),("Adam",55),("Lisa",60),("Will",71),("Mark",67)]

the1Mk :: [CS1] -> [(String, Int)]
the1Mk db = map takeNameAndTHE1 db
    where takeNameAndTHE1 s = (name(s), the1(s))

the1Mk' :: [CS1] -> [(String, Int)]
the1Mk' ss = [(name s, the1 s) | s <- ss]
