{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO as TIO
import Data.Char (isDigit, isLetter)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn, dropBlanks)

main = do
    input <- lines <$> readFile "input"
    let [stacks', commands] = sequence [init . fst, tail . snd] $ span (/= "") input
    let stacks = map (dropWhile (== "")) $ transpose $ map (map (filter isLetter) . chunksOf 4) stacks'
    let commandValues = map (map (read :: String -> Int) . filter (not . (`elem` ["move", "from", "to"])) . words) commands
    mapM_ (print . (map head . process stacks commandValues)) [reverse, id]
    where
        updateStackAt i stack stacks = xs ++ [stack] ++ tail ys where (xs,ys) = splitAt i stacks
        processCommand num firstStackI secStackI stacks processMode = updateStackAt secStackI secStack $ updateStackAt firstStackI firstStackTail stacks
            where
                (firstStackTop, firstStackTail) = splitAt num (stacks !! firstStackI)
                secStack = processMode firstStackTop ++ stacks !! secStackI
        process stacks commands processMode = foldl (\s c -> processCommand (head c) ((c !! 1) -1) ((c !! 2) -1) s processMode) stacks commands
