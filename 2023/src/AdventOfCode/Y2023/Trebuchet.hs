module AdventOfCode.Y2023.Trebuchet (day) where

import qualified AdventOfCode
import qualified Data.Char as Char


day :: AdventOfCode.Day
day =
    AdventOfCode.day "Trebuchet" part1 part2


part1 :: String -> Int
part1 =
    sum . fmap calibration . lines


part2 :: String -> Int
part2 =
    const 0


calibration :: String -> Int
calibration line =
    let
        digits = filter Char.isDigit line
        firstDigit = Char.digitToInt $ head digits
        lastDigit = Char.digitToInt $ head (reverse digits)
    in
        firstDigit * 10 + lastDigit
