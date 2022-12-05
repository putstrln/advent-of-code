import Data.List.Split (splitOn)
import Data.Char (isDigit, digitToInt)

main = readFile "input"
    >>= print
        . sequence [
            length . filter fullyContains,
            length . filter overlaps
        ]
        . map (map readIntFromPair . splitOn ",")
        . lines
        where
            readIntFromPair = tuple . map (read :: String -> Int) . splitOn "-"
            fullyContains a = fullyContains' x y || fullyContains' y x where
                fullyContains' x' y' = fst x' <= fst y' && snd x' >= snd y'
                x = head a
                y = last a
            overlaps a = overlaps' x y || overlaps' y x where
                overlaps' x' y' = fst x' >= fst y' && fst x' <= snd y'
                x = head a
                y = last a
            tuple [x,y] = (x,y)