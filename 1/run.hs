{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import qualified Data.Text.Lazy.Read as TR
import Data.Either
import Data.List

-- main = do
--     input <- TextIO.readFile "./input"
--     let sums = map ((sum . map fst . rights . map TR.decimal) . T.lines) (T.splitOn "\n\n" input)
--     print $ foldr max 0 sums

-- main = TextIO.readFile "./input"
--     >>= print
--         . foldr (max . ((sum . map fst . rights . map TR.decimal) . T.lines)) 0
--         . T.splitOn "\n\n"

main = TextIO.readFile "./input"
    >>= print
        . sum . take 3 . reverse . sort
        . map ((sum . map fst . rights . map TR.decimal) . T.lines)
        . T.splitOn "\n\n"