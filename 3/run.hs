import Data.List (intersect)
import Data.Char (isLower, ord, isUpper)
import Data.List.Split

main = readFile "input"
    >>= print
        . sequence [
            sum . map ((priority . head . uncurry intersect) . half),
            sum . map (priority . head . foldr1 intersect) . chunksOf 3
        ]
        . lines
        where
            half s = splitAt ((length s + 1) `div` 2) s
            priority c = ord c  - if isLower c then 96 else 38