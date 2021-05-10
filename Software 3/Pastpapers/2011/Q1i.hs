module Q1i where --

type Word = String
type Grid = [String]
data Direction = Right | Down | Downright | Upright | None

find :: Grid -> Word -> Direction
find g w 
    | w `within` g = Right
    | w `within` transpose g = Right
    | w `within` diagonals g = Right
    | w `within` diagonals (reverse g) = Right
    | otherwise = None
    where
        w `within` ss = any (subString w) ss



-- 前缀 中缀 后缀
-- function x x
