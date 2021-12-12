module AdventOfCode.Y2021.SonarSweep
    ( day
    ) where

import Data.List as List
import qualified AdventOfCode


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Sonar Sweep" part1 part2
    where
        part1 =
            countIncreases . parse

        part2 =
            countWindowIncreases . parse

        parse =
            fmap read . lines


countIncreases :: [Int] -> Int
countIncreases [] =
    0
countIncreases [ _ ] =
    0
countIncreases (a : b : rest)
    | b > a =
        1 + countIncreases (b : rest)
    | otherwise =
        countIncreases (b : rest)


countWindowIncreases :: [Int] -> Int
countWindowIncreases list =
    countIncreases $
        concat $
            List.transpose [ window 0, window 1, window 2, window 3 ]
    where
        window n =
            sum3 (drop n list)

        sum3 (a : b : c : _ : xs) =
            ((a + b + c) : sum3 xs)
        sum3 [ a, b, c ] =
            [ a + b + c ]
        sum3 _ =
            []
