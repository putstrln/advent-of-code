import Data.List (sort)
main = readFile "input"
    >>= print . sequence [marker 0 4, marker 0 14] where
        unique s = and $ zipWith (/=) s' (drop 1 s') where s' = sort s
        marker i uniqueNum xs = if unique (take uniqueNum xs) then i + uniqueNum
                                else marker (i+1) uniqueNum (tail xs)
