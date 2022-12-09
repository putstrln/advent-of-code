{-# LANGUAGE OverloadedStrings #-}
import Data.Char (digitToInt)
import Data.List (transpose)

process m  = fst $ foldl visible ([], 0) m where
    visible (acc, i) r = (zipWith (curry visible') r [0..] : acc, i + 1) where
        visible' (n, col) = visibleRow || visibleCol where
            visibleRow = visibleFromSide n col r
            visibleCol = visibleFromSide n i (mCols !! col)
    mCols = cols m
    visibleFromSide n i nums = isMax n l || isMax n (drop 1 r) where
        isMax n nums = null nums || (n > maximum  nums)
        (l, r) = splitAt i nums

process' m  = fst $ foldl visible ([], 0) m where
    visible (acc, i) r = (acc ++ [zipWith (curry visible') r [0..]], i + 1) where
        visible' (n, col) = leftRight * topBottom where
            leftRight = getTreeDistance n col r
            topBottom = getTreeDistance n i (mCols !! col)
    mCols = cols m
    getTreeDistance n i nums = leftOrTop * rightOrBottom where
        (l, r) = splitAt i nums
        leftOrTop = getVisibleTreesFromSide n (reverse l)
        rightOrBottom = getVisibleTreesFromSide n (drop 1 r)
    getVisibleTreesFromSide n nums = length $ takeWhile' (< n) nums
    takeWhile' _ [] = []
    takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else [x]

cols matrix = fst $ foldr (\_ (acc, c) -> (acc ++ [getCol matrix c], c + 1)) ([], 0) $ head matrix
    where getCol matrix i = map (!! i) matrix

main = readFile "input"
    >>= print
        . sequence [
            length . filter (True ==) . concat . process,
            maximum . concat . process'
        ]
        . map (map digitToInt)
        . lines