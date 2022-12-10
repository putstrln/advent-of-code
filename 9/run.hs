{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Set as S

data Dir = L | R | U | D deriving (Show, Read)
data Cell = Cell Int Int  deriving (Show, Eq, Ord)

step :: [Cell] -> Int -> [Cell]
step hs n = take (n+1) (iterate (scanl1 follow) hs) !! n

move :: Cell -> Dir -> Cell
move (Cell x y) dir = case dir of
    U -> Cell x  (y + dist)
    D -> Cell x (y - dist)
    L -> Cell (x - dist) y
    R -> Cell (x + dist) y
    where dist = 1

follow :: Cell -> Cell -> Cell
follow t@(Cell trow tcol) (Cell hrow hcol) =
    if abs rdiff <= 1 && abs cdiff <= 1 then t
    else Cell (trow + signum rdiff) (tcol + signum cdiff)
    where
        rdiff = hrow - trow
        cdiff = hcol - tcol

parse = map (parse' . words)
parse' lineItems = (read $ head lineItems, read $ last lineItems)
main = readFile "input"
    >>= print
        . sequence [
            S.size . S.fromList . flip step 1,
            S.size . S.fromList . flip step 9
        ]
        . scanl move (Cell 0 0)
        . foldr (\(dir, dist) ms -> replicate dist dir ++ ms) []
        . parse
        . lines