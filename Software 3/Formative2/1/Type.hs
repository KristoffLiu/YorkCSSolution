module Type where --

newtype Age = Age Int deriving (Show)
newtype Age' = Age' (String -> Int)

getAge :: Int -> Age
getAge a = Age a

getAge' :: String -> Int
getAge' "Kristoff" = 21
getAge' _ = 100

