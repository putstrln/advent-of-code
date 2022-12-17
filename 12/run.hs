{-# LANGUAGE TupleSections #-}
import Data.Array as A
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Char (ord)
import Control.Monad (liftM2)
import Data.Set (Set)

data Step = Start | End | Step Char deriving (Show, Eq)
type Pos = (Int, Int)
type PosDistance = (Pos, Int)
type Matrix = Array Pos Step

matrix :: [[Step]] -> Matrix
matrix ss = listArray (lower, upper) $ concat ss where
    lower = (0,0)
    upper = (length ss - 1, length (head ss) - 1)

parse :: String -> [[Step]]
parse = map (map parse') . lines where
    parse' c = case c of
        'S' -> Start
        'E' -> End
        _ -> Step c

neighborsToStep :: Matrix -> Pos -> [Pos]
neighborsToStep m pos@(x,y) = filter (liftM2 (&&) inBounds canStepOnNeighbor) upDownRightLeftNeighbors where
    inBounds = A.inRange (A.bounds m)
    canStepOnNeighbor = canStep (m ! pos) . (m !)
    upDownRightLeftNeighbors = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    canStep Start (Step e) = e <= 'b'
    canStep (Step e) End = e >= 'y'
    canStep (Step a) (Step b) = b <= succ a
    canStep _ _ = False

bfs' :: Matrix -> Step -> [PosDistance] -> Set Pos -> Int
bfs' m end ((currPos, posDist):rest) seen
  | m ! currPos == end = posDist
  | currPos `Set.member` seen = bfs' m end rest seen
  | otherwise = bfs' m end posDist' seen'
  where
      neighbors = neighborsToStep m currPos
      seen' = Set.insert currPos seen
      newNeighbors = filter (`Set.notMember` seen) neighbors
      posDist' = rest ++ map (, posDist + 1) newNeighbors
bfs' _ _ _ _ = -1

climb :: Matrix -> Pos -> Step -> Int
climb m sPos e = bfs' m e [(sPos, 0)] Set.empty

findStep :: Step -> Matrix -> Pos
findStep s = fst . fromJust . find ((s ==) . snd) . A.assocs

findAPos = map fst . filter ((Step 'a' ==) . snd) . A.assocs

main = do
    input <- readFile "input"
    let steps = parse input
    let m = matrix steps
    print $ climb m (findStep Start m) End
    print $ minimum $ filter (> 0) $ map (\pos -> climb m pos End) (findAPos m)
