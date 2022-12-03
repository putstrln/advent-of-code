
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Control.Monad (liftM2)

data Result = Win | Loss | Draw deriving (Eq)
data Hand = Rock | Paper | Scissors deriving (Read, Show, Enum, Eq)

combos = [
    ((Rock,Rock),Draw),((Rock,Paper),Win),((Rock,Scissors),Loss),
    ((Paper,Paper),Draw),((Paper,Rock),Loss),((Paper,Scissors),Win),
    ((Scissors,Scissors),Draw),((Scissors,Rock),Win),((Scissors,Paper),Loss)
    ]

firstColMappings = [("A", Rock), ("B", Paper), ("C", Scissors)] :: [(String, Hand)]

class Round a where
    playStrat :: a -> (Result, Hand)

instance Round (Hand, Hand) where
    playStrat hands = (snd . fromJust $ find (\x -> fst x == hands) combos, snd hands)

instance Round (Hand, Result) where
    playStrat (hand, result) =
        (result, snd . fst . fromJust $ find isCombo combos)
           where isCombo (hands, status) = fst hands == hand && status == result

parse secColMappings = liftM2 (,) parseFirstCol (parseCol secColMappings . last)
play :: Round a => a -> Int
play = calcPoints . playStrat
parseCol mappings c = snd $ fromJust $ find (\x -> fst x == c) mappings
parseFirstCol = parseCol firstColMappings . head

calcPoints (result, hand) = case result of
    Win -> 6 + handPoint
    Loss -> 0 + handPoint
    Draw -> 3 + handPoint
    where handPoint = 1 + fromEnum hand

main = do
    input <- map words . lines <$> readFile "input"
    let runGame playGame mappings = sum . map (playGame . parse mappings) $ input
    let playPart1 = play :: (Hand, Hand) -> Int
    let playPart2 = play :: (Hand, Result) -> Int
    print $ runGame playPart1 ([("X", Rock), ("Y", Paper), ("Z", Scissors)] :: [(String, Hand)])
    print $ runGame playPart2 ([("X", Loss), ("Y", Draw), ("Z", Win)] :: [(String, Result)])